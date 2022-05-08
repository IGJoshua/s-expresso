(ns brick-breaker.core
  (:require
   [cljsl.compiler :as sl]
   [clojure.core.matrix :as mat]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.java.io :as io]
   [net.cgrand.xforms :as xf]
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

(mat/set-current-implementation :vectorz)

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

(defn- default-log-fn
  [info-map message]
  (log/warn info-map "UNKNOWN SEVERITY:" message))
(def glenum->log-function
  "Map from GLEnum values to logging functions representing severity of a message."
  {GL45/GL_DEBUG_SEVERITY_NOTIFICATION #(log/info %1 %2)
   GL45/GL_DEBUG_SEVERITY_LOW #(log/warn %1 %2)
   GL45/GL_DEBUG_SEVERITY_MEDIUM #(log/error %1 %2)
   GL45/GL_DEBUG_SEVERITY_HIGH #(log/fatal %1 %2)})

(def glenum->source-description
  "Map from GLEnum values to strings representing the source of a message."
  {GL45/GL_DEBUG_SOURCE_API "OpenGL API"
   GL45/GL_DEBUG_SOURCE_WINDOW_SYSTEM "Windowing System"
   GL45/GL_DEBUG_SOURCE_SHADER_COMPILER "Shader Compiler"
   GL45/GL_DEBUG_SOURCE_THIRD_PARTY "Third Party Integration"
   GL45/GL_DEBUG_SOURCE_APPLICATION "Application-generated"
   GL45/GL_DEBUG_SOURCE_OTHER "Unspecified"})

(def glenum->type-description
  "Map from GLEnum values to strings representing the type of message."
  {GL45/GL_DEBUG_TYPE_ERROR "Error"
   GL45/GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR "Deprecated"
   GL45/GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR "Undefined Behavior"
   GL45/GL_DEBUG_TYPE_PORTABILITY "Portability"
   GL45/GL_DEBUG_TYPE_PERFORMANCE "Performance"
   GL45/GL_DEBUG_TYPE_PUSH_GROUP "Push Group"
   GL45/GL_DEBUG_TYPE_POP_GROUP "Pop Group"
   GL45/GL_DEBUG_TYPE_OTHER "Unspecified"})

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
         (invoke [_this source type id severity length message _user-param]
           (let [log-fn (get glenum->log-function severity default-log-fn)]
             (log-fn (str "{"
                          "Source=" (get glenum->source-description source "Unknown") ", "
                          "Type=" (get glenum->type-description type "Unknown") ", "
                          "Id=" id
                          "}")
                     (GLDebugMessageCallback/getMessage length message)))))
       0))))

(defonce ^{:doc "An atom with a vector of input events since the last frame."}
  input-events (atom []))
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
  (case (:key event)
    (:left :right)
    (update scene ::direction (if (#{:press :repeat} (:action event))
                                (fnil conj #{})
                                disj)
            (:key event))

    :escape
    (assoc scene ::e/should-close? true)

    scene))

(defn input
  [scene _dt]
  (let [[events _] (reset-vals! input-events [])]
    (when (seq events)
      (println events))
    (reduce input-event scene events)))

(defn move-paddle
  [world dt]
  (let [paddle-id (::player world)
        paddle (get-in world [::ecs/entities paddle-id])
        paddle-x (mat/mget (::position paddle) 0)
        direction (and (= 1 (count (::direction world)))
                       (first (::direction world)))]
    (if (and direction
             (case direction
               :left (> paddle-x (- (/ (::width world) 2)))
               :right (< paddle-x (/ (::width world) 2))))
      (update-in world [::ecs/entities paddle-id ::position]
                 mat/add
                 (mat/array [(case direction
                               :left (* dt -1 (::speed paddle 10))
                               :right (* dt (::speed paddle 10)))
                             0]))
      world)))

(defsystem move-balls [::velocity ::position]
  [_ _ ball dt]
  (update ball ::position mat/add (mat/scale (::velocity ball) dt)))

(defsystem recover-lost-balls [::velocity ::position]
  [world _ ball _]
  (if (< (mat/mget (::position ball) 1) (- (::height world)))
    (assoc ball ::position (mat/array [0 (- (/ (::height world) 2) 2)]))
    ball))

(defsystem wall-bounce [::velocity ::position]
  [world _ ball _]
  (let [horiz-dist (/ (::width world) 2)
        vert-dist (/ (::height world) 2)
        [x y] (seq (::position ball))]
    (cond-> ball
      (pos? (- (abs x) horiz-dist))
      (-> (update ::velocity mat/mul (mat/array [-1 1]))
          (update ::position mat/sub (mat/array [(* 2 (math/signum x) (- (abs x) horiz-dist)) 0])))

      (> y vert-dist)
      (-> (update ::velocity mat/mul (mat/array [1 -1]))
          (update ::position mat/sub (mat/array [0 (* 2 (- y vert-dist))]))))))

(defsystem paddle-bounce [::velocity ::position]
  [world _ ball _]
  (let [paddle (get-in world [::ecs/entities (::player world)])
        [paddle-x paddle-y] (seq (::position paddle))
        [ball-x ball-y] (seq (::position ball))
        dist-from-paddle (- ball-x paddle-x)]
    (if (and (neg? ball-y)
             (< (abs (- ball-y paddle-y)) 0.3)
             (< (abs dist-from-paddle) (::width paddle)))
      (-> ball
          (update ::velocity mat/mul (mat/array [1 -1.05]))
          (update ::velocity mat/add (mat/array [(* (/ dist-from-paddle (::width paddle))
                                                    (rand (::spread paddle)))
                                                 0])))
      ball)))

(def systems [#'input #'move-paddle [#'move-balls #'recover-lost-balls #'wall-bounce #'paddle-bounce]])

(def quad-mesh-data {:vertices [{:pos [-0.5 -0.5] :uv [0 0]}
                                {:pos [ 0.5 -0.5] :uv [1 0]}
                                {:pos [ 0.5  0.5] :uv [1 1]}
                                {:pos [-0.5  0.5] :uv [0 1]}]})

(def quad-mesh-layout {:vertex-layouts [{:attrib-layouts [{:name :pos
                                                           :type :float
                                                           :count 2}
                                                          {:name :uv
                                                           :type :half-float
                                                           :count 2}]
                                         :interleaved true}]
                       :instance-layouts [{:attrib-layouts [{:name :pos
                                                             :type :float
                                                             :count 2}]}]
                       :element-type :triangle-fan})

(def quad-mesh
  (r/resolver [mesh (m/pack-verts quad-mesh-layout quad-mesh-data)]
    (m/make-mesh quad-mesh-layout mesh)))

(sl/defparam vert-pos "vec2"
  :layout {"location" 0})
(sl/defparam vert-uv "vec2"
  :layout {"location" 1})
(sl/defparam instance-pos "vec2"
  :layout {"location" 2})

(sl/defparam frag-uv "vec2")

(sl/defuniform cam-pos "vec2")
(sl/defuniform zoom "float")
(sl/defuniform dims "vec2")
(sl/defuniform aspect-ratio "float")

(sl/defshader vert-shader
  {vert-pos :in
   vert-uv :in
   instance-pos :in
   frag-uv :out}
  (set! frag-uv vert-uv)
  (let [^"vec2" pos (* (- (+ (* vert-pos dims) instance-pos) cam-pos) zoom)]
    (set! gl_Position (vec4 (:x pos) (* (:y pos) aspect-ratio) 0 1))))

(sl/defuniform sam "sampler2D")
(sl/defparam target-color "vec4")

(sl/defshader frag-shader
  {frag-uv :in
   target-color :out}
  (let [^"vec4" tex-color (set! target-color (texture sam frag-uv))]
    ;; HACK(Joshua): use `cond` because there's a bug with `if` in cljsl
    (cond
      (> (:a tex-color) 0.5) (set! target-color tex-color)
      :else (discard))))

(def sprite-shader
  #(sh/make-shader-program-from-sources
    [{:stage :vertex
      :source (::sl/source vert-shader)}
     {:stage :fragment
      :source (::sl/source frag-shader)}]))

(defn texture-resolver
  [sprite-key]
  (r/resolver [image (tex/load-image (str "assets/" (get-in asset-files (cons :images sprite-key))))]
    ^{`res/free (fn [{::keys [data]}] (res/free data))}
    {::data (tex/make-texture {:internal-format :rgba8
                               :dimensions (:dimensions image)}
                              {:format :rgba
                               :data-type :unsigned-byte
                               :data (:data image)})
     ::dimensions (:dimensions image)}))

(defn sprite-batch
  [sprite-key camera-pos camera-zoom scale positions]
  (reify r/RenderOp
    (op-deps [_]
      {::quad quad-mesh
       ::sprite-shader sprite-shader
       [::texture sprite-key] (texture-resolver sprite-key)
       ::batch-instance-loaded? #(volatile! false)})
    (apply-op! [_ {{::keys [quad sprite-shader batch-instance-loaded?] texture [::texture sprite-key]} ::r/resources}]
      (when (and quad batch-instance-loaded? (not @batch-instance-loaded?))
        (mem/with-stack-allocator
          (m/set-instance-buffer-contents! quad 0 (mem/alloc-bytes (* 2 Float/SIZE 512))))
        (vreset! batch-instance-loaded? true))
      (when (and quad sprite-shader texture)
        (sh/with-shader-program sprite-shader
          (tex/with-texture (::data texture) 0
            (sh/upload-uniform-int sprite-shader (::sl/ident sam) 0)
            (let [[x y] (map (partial * scale) (::dimensions texture))]
              (sh/upload-uniform-float sprite-shader (::sl/ident dims) x y))
            (sh/upload-uniform-float sprite-shader (::sl/ident zoom) camera-zoom)
            (sh/upload-uniform-float sprite-shader (::sl/ident aspect-ratio) (/ 4 3))
            (let [[x y] (seq camera-pos)]
              (sh/upload-uniform-float sprite-shader (::sl/ident cam-pos) x y))
            (mem/with-stack-allocator
              (let [last-batch
                    (mem/with-stack-allocator
                      (let [instance-positions (mem/alloc-bytes (* 2 Float/SIZE 512))]
                        (loop [[pos & more] (partition-all 512 positions)]
                          (if (seq more)
                            (do
                              (mem/put-seq instance-positions
                                           (->> pos
                                                (mapcat seq)
                                                (map float)))
                              (.flip instance-positions)
                              (m/set-instance-buffer-sub-contents! quad 0 0 instance-positions)
                              (m/draw-mesh quad 512)
                              (recur more))
                            (vec pos)))))
                    instance-positions (mem/alloc-bytes (* 2 Float/SIZE (count last-batch)))]
                (mem/put-seq instance-positions
                             (->> last-batch
                                  (mapcat seq)
                                  (map float)))
                (.flip instance-positions)
                (m/set-instance-buffer-sub-contents! quad 0 0 instance-positions)
                (m/draw-mesh quad (count last-batch))))))))))

(defn draw-sprites
  [game-state]
  (let [camera-pos (mat/array [0 0])
        zoom (/ 15)
        scale (/ 50)]
    (sequence
     (comp (map val)
           (filter ::sprite-path)
           (xf/by-key ::sprite-path (comp (map ::position)
                                          (xf/into [])))
           (map #(sprite-batch (first %) camera-pos zoom scale (second %))))
     (::ecs/entities game-state))))

(defn clear-screen
  [game-state]
  (list
   (reify r/RenderOp
     (op-deps [_]
       {})
     (apply-op! [_ _]
       (let [[r g b] (::background-color game-state)]
         (GL45/glClearColor (float r) (float g) (float b) 1.0)
         (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT)))))))

(def render-systems [#'clear-screen #'draw-sprites])

(defn lerp
  [a b t]
  (doto (mat/sub b a)
    (mat/scale! t)
    (mat/add! a)))

(defn interpolate
  [game-state last-state factor]
  (try (let [last-entities (::ecs/entities last-state)]
         (update game-state ::ecs/entities
                 (fn [entities]
                   (persistent!
                    (reduce-kv
                     (fn [m k v]
                       (assoc! m k
                               (if-some [old-pos (get-in last-entities [k ::position])]
                                 (update v ::position (partial lerp old-pos) factor)
                                 v)))
                     (transient {}) entities)))))
       (catch Exception e
         (println e)
         game-state)))

(def init-game-state
  (let [player (ecs/next-entity-id)
        ball (ecs/next-entity-id)]
    {::ecs/entities {player {::position (mat/array [0 -10])
                             ::sprite-path [:paddle]
                             ::width 1.2
                             ::spread 3
                             ::speed 10}
                     ball {::position (mat/array [0 0])
                           ::sprite-path [:ball]
                           ::velocity (mat/array [(* 10 (- (rand) 0.5)) 5])}}
     ::ecs/systems #'systems
     ::ecs/events []
     ::e/events []
     ::e/event-handler #'handle-render-event
     ::r/systems #'render-systems
     ::r/interpolator #'interpolate
     ::background-color [0.1 0.15 0.2]
     ::width 25
     ::height 20
     ::player player}))

(def init-render-state
  {::r/resolvers {}
   ::r/resources {}})

(defn run
  []
  (let [wnd (make-window window-opts)]
    (try (let [[_game-state render-state]
               (e/start-engine wnd init-game-state
                               (assoc init-render-state ::e/step (double (/ 165)))
                               (double (/ 100)))]
           (r/shutdown-state render-state))
         (finally (shutdown-window wnd)))))

(defn -main
  []
  (init)
  (run)
  (shutdown)
  (shutdown-agents))
