(ns examples.triangle
  (:require
   [cljsl.compiler :as c]
   [examples.window :as e.w]
   [s-expresso.engine :as e]
   [s-expresso.ecs :as ecs]
   [s-expresso.memory :refer [with-stack-allocator with-heap-allocator]]
   [s-expresso.mesh :as m]
   [s-expresso.render :as r]
   [s-expresso.resource :refer [with-free]]
   [s-expresso.shader :as sh]))

(c/defparam a-pos "vec3"
  :layout {"location" 0})
(c/defuniform a-col "vec3")
(c/defparam col "vec3")

(c/defshader vert-source
  {a-pos :in
   a-col :in
   col :out}
  (set! col a-col)
  (set! gl_Position (vec4 (:xyz a-pos) 1.0)))

(def vert-shader
  {:source (::c/source vert-source)
   :stage :vertex})

(c/defparam frag-color "vec4")

(c/defshader frag-source
  {col :in
   frag-color :out}
  (set! frag-color (vec4 (:xyz col) (float 1.0))))

(def frag-shader
  {:source (::c/source frag-source)
   :stage :fragment})

(def tri-mesh-data {:vertices [{:pos [-0.5 -0.5 0.0]}
                               {:pos [0.5 -0.5 0.0]}
                               {:pos [0.0 0.5 0.0]}]})

(def pos-mesh-layout {:buffer-layouts [{:attrib-layouts [{:name :pos
                                                          :type :float
                                                          :count 3}]}]
                      :element-type :triangles})

(defn mesh-resolver
  [mesh layout]
  #(with-heap-allocator
     (future
       (let [mesh (m/pack-verts layout mesh)]
         (delay (m/make-mesh layout mesh))))))

(defn shader-program-resolver
  [sources]
  #(sh/make-shader-program-from-sources sources))

(def ^:private shader-program (shader-program-resolver [vert-shader frag-shader]))
(def ^:private triangle-mesh (mesh-resolver tri-mesh-data pos-mesh-layout))

(defn render-entity
  [entity]
  (reify r/RenderOp
    (op-deps [_]
      {::triangle triangle-mesh
       ::shader-program shader-program})
    (apply-op! [_ {{::keys [triangle shader-program]} ::r/resources}]
      (when (and triangle shader-program)
        (sh/with-shader-program shader-program
          (apply sh/upload-uniform-float shader-program (c/sym->ident `a-col) (::color entity))
          (m/draw-mesh triangle))))))

(defn- draw-mesh
  [game-state]
  (->> (::ecs/entities game-state)
       vals
       (filter ::position)
       (map render-entity)))

(defn- ingest-input
  [game-state _dt]
  (let [[input-events] (reset-vals! e.w/input-events [])
        input-events (group-by :device input-events)
        mouse-pos (:pos
                   (last
                    (filter (comp #{:move} :action)
                            (:mouse input-events))))
        close? (seq
                (filter (comp #{:close} :action)
                        (:window input-events)))]
    (cond-> game-state
      mouse-pos (assoc :mouse-pos mouse-pos)
      close? (assoc ::e/should-close? close?))))

(def ^:private init-game-state
  {::ecs/entities {(ecs/next-entity-id) {::position [0 0]
                                         ::color [1 0 0]}}
   ::ecs/systems [#'ingest-input]
   ::ecs/events []
   ::r/systems [#'draw-mesh]})

(def ^:private init-render-state
  {::r/resolvers {}
   ::r/resources {}})

(defn run-sim
  [window game-state render-state]
  (let [[_game-state render-state] (e/start-engine window game-state render-state (/ 60))]
    (r/shutdown-state render-state))
  window)

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (run-sim init-game-state init-render-state)
      e.w/shutdown-window)
  (e.w/shutdown))
