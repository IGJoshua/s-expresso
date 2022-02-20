(ns brick-breaker.core
  (:require
   [cljsl.compiler :as sl]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [s-expresso.audio :as sfx]
   [s-expresso.engine :as e]
   [s-expresso.ecs :as ecs :refer [defsystem]]
   [s-expresso.memory :as mem]
   [s-expresso.mesh :as m]
   [s-expresso.render :as r]
   [s-expresso.resource :as res]
   [s-expresso.shader :as sh]
   [s-expresso.texture :as tex]
   [s-expresso.window :as wnd]
   [taoensso.timbre :as log])
  (:import
   (java.io PushbackReader)
   (org.lwjgl.opengl
    GL GL45 GLDebugMessageCallback GLDebugMessageCallbackI)))

(def asset-files (edn/read (PushbackReader. (io/reader (io/resource "assets.edn")))))

(defonce connected-joysticks (atom #{}))

(defn init
  []
  (wnd/init-glfw {:error-callback #(log/error (str "GLFW Error: " %1 "\n" %2))})
  (wnd/set-joystick-callback
   (fn [jid event]
     (case event
       :connected (swap! connected-joysticks conj jid)
       :disconnected (swap! connected-joysticks disj jid))))
  (sfx/init-openal))

(defn shutdown
  []
  (sfx/shutdown-openal)
  (wnd/shutdown-glfw))

(defn enable-debug-logging!
  []
  (let [flags (int-array 1)]
    (GL45/glGetIntegerv GL45/GL_CONTEXT_FLAGS flags)
    (when-not (zero? (bit-and GL45/GL_CONTEXT_FLAG_DEBUG_BIT
                              (first flags)))
      (GL45/glEnable GL45/GL_DEBUG_OUTPUT)
      (GL45/glEnable GL45/GL_DEBUG_OUTPUT_SYNCHRONOUS)
      (GL45/glDebugMessageControl GL45/GL_DONT_CARE
                                  GL45/GL_DONT_CARE
                                  GL45/GL_DONT_CARE
                                  (int-array 0)
                                  true)
      (GL45/glDebugMessageCallback
       (reify GLDebugMessageCallbackI
         (invoke [this source type id severity length message user-param]
           (log/debug (GLDebugMessageCallback/getMessage length message))))
       0))))

(defonce input-events (atom []))
(def window-opts
  {:key-callback (fn [_window key _scancode action mods]
                   (swap! input-events (fnil conj [])
                          {:device :keyboard
                           :key key
                           :action action
                           :mods mods}))
   :cursor-pos-callback (fn [_window xpos ypos]
                          (swap! input-events (fnil conj [])
                                 {:device :mouse
                                  :action :move
                                  :pos [xpos ypos]}))
   :mouse-button-callback (fn [_window button action mods]
                            (swap! input-events (fnil conj [])
                                   {:device :mouse
                                    :button button
                                    :action action
                                    :mods mods}))
   :request-close-callback (fn [window]
                             (wnd/window-should-close window false)
                             (swap! input-events conj
                                    {:device :window
                                     :action :close}))
   :title "Brick Breaker"})

(defn make-window
  [opts]
  (let [wnd (-> (wnd/make-window opts)
                (wnd/make-context-current-to-window)
                (wnd/center-window)
                (wnd/show-window))]
    (wnd/set-vsync true)
    (GL/createCapabilities)
    (enable-debug-logging!)
    wnd))

(defn shutdown-window
  [wnd]
  (res/free wnd))

(defmulti handle-render-event
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [render-state event]
    (:type event)))

(defmulti input-event
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [scene event]
    (:device event)))

(defmethod input-event :default
  [scene _event]
  scene)

(defmethod input-event :window
  [scene event]
  (case (:action event)
    :close (assoc scene ::e/should-close? true)

    scene))

(defmethod input-event :keyboard
  [scene event]
  (case (:button event)
    (:left :right)
    (update scene :direction (if (#{:press} (:action event))
                               conj
                               disj)
            (:button event))

    :escape
    (assoc scene ::e/should-close? true)

    scene))

(defn input
  [scene _dt]
  (let [[events _] (reset-vals! input-events [])]
    (when (seq events)
      (println events))
    (reduce input-event scene events)))

(def systems [#'input])

(def quad-mesh-data {:vertices [{:pos [-0.5 -0.5] :uv [0 0]}
                                {:pos [ 0.5 -0.5] :uv [1 0]}
                                {:pos [ 0.5  0.5] :uv [1 1]}
                                {:pos [-0.5  0.5] :uv [0 1]}]})

(def quad-mesh-layout {:buffer-layouts [{:attrib-layouts [{:name :pos
                                                           :type :float
                                                           :count 2}
                                                          {:name :uv
                                                           :type :half-float
                                                           :count 2}]
                                         :interleaved true}]
                       :element-type :triangle-fan})

(def quad-mesh
  #(mem/with-heap-allocator
     (future
       (let [mesh (m/pack-verts quad-mesh-layout quad-mesh-data)]
         (delay (m/make-mesh quad-mesh-layout mesh))))))

(sl/defparam vert-pos "vec2"
  :layout {"location" 0})
(sl/defparam vert-uv "vec2"
  :layout {"location" 1})

(sl/defparam frag-uv "vec2")

(sl/defuniform pos "vec2")
(sl/defuniform scale "float")

(sl/defshader vert-shader
  {vert-pos :in
   vert-uv :in
   frag-uv :out}
  (set! frag-uv vert-uv)
  (set! gl_Position (/ (vec4 vert-pos 0 1) scale)))

(sl/defuniform sam "sampler2D")
(sl/defparam target-color "vec4")

(sl/defshader frag-shader
  {frag-uv :in
   target-color :out}
  (set! target-color (texture sam frag-uv)))

(def sprite-shader
  #(sh/make-shader-program-from-sources [vert-shader frag-shader]))

(defn texture-resolver
  [sprite-key]
  #(future
     (let [image (tex/load-image (get-in asset-files (cons :images sprite-key)))]
       (delay
         (tex/make-texture {:internal-format :rgb8
                            :dimensions (:dimensions image)}
                           {:format :rgb
                            :data-type :unsigned-byte
                            :data (:data image)})))))

(defn sprite
  [sprite-key zoom position]
  (reify r/RenderOp
    (op-deps [_]
      {::quad quad-mesh
       ::sprite-shader sprite-shader
       [::texture sprite-key] (texture-resolver sprite-key)})
    (apply-op! [_ {{::keys [quad sprite-shader] texture [::texture sprite-key]} ::r/resources}]
      (when (and quad sprite-shader texture)
        (sh/with-shader-program sprite-shader
          (tex/with-texture texture 0
            (sh/upload-uniform-int sprite-shader (sl/sym->ident `sam) 0)
            (sh/upload-uniform-floats sprite-shader 2 (sl/sym->ident `pos) position)
            (sh/upload-uniform-float sprite-shader (sl/sym->ident `scale) (/ zoom))
            (m/draw-mesh quad)))))))

(def render-systems [])

(def init-game-state
  (let [player (ecs/next-entity-id)]
    {::ecs/entities {player {::position [0 -10]}}
     ::ecs/systems #'systems
     ::ecs/events []
     ::e/events []
     ::e/event-handler #'handle-render-event
     ::r/systems #'render-systems}))

(def init-render-state
  {::r/resolvers {}
   ::r/resources {}})

(defn run
  []
  (let [wnd (make-window window-opts)]
    (try (let [[_game-state render-state]
               (e/start-engine wnd init-game-state init-render-state (/ 100))]
           (r/shutdown-state render-state))
         (finally (shutdown-window wnd)))))

(defn -main
  []
  (init)
  (run)
  (shutdown))
