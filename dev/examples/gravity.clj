(ns examples.gravity
  (:require
   [cljsl.compiler :as c]
   [examples.window :as e.w]
   [examples.triangle :as e.t]
   [s-expresso.mesh :as m]
   [s-expresso.render :as r]
   [s-expresso.shader :as sh]
   [s-expresso.ecs :as ecs :refer [defsystem]]
   [s-expresso.physics.dynamics :as d]
   [s-expresso.physics.constraints :as constraint]))

(def gravity-constant 0.0015 #_(* 6.674 (Math/pow 10 -11)))

(defsystem gravity [::d/mass ::d/position]
  [scene entity-id entity dt]
  (let [other-bodies (filter (comp (partial some identity)
                                   (juxt ::d/position
                                         ::d/mass)
                                   second)
                             (::ecs/entities scene))
        forces (for [[id body] other-bodies
                     :when (not= id entity-id)]
                 (constraint/gravitational-force (::d/position entity)
                                                 (::d/mass entity)
                                                 (::d/position body)
                                                 (::d/mass body)
                                                 gravity-constant))]
    (reduce d/add-force entity forces)))

(defsystem step-body [::d/mass ::d/position ::d/velocity]
  [_ _ entity dt]
  (d/step-body entity dt))

(c/defparam v-pos "vec3"
  :layout {"location" 0})
(c/defparam color "vec3")

(c/defuniform obj-col "vec3")
(c/defuniform obj-pos "vec2")

(c/defshader vert-source
  {v-pos :in
   color :out}
  (set! gl_Position (* (vec4 (+ (vec3 obj-pos 0) v-pos) 1)
                       (vec4 0.3 0.3 0 1)))
  (set! color obj-col))

(def vert-shader
  {:source (::c/source vert-source)
   :stage :vertex})

(c/defparam frag-color "vec4")

(c/defshader frag-source
  {color :in
   frag-color :out}
  (set! frag-color (vec4 color 1)))

(def frag-shader
  {:source (::c/source frag-source)
   :stage :fragment})

(def tri-mesh-data {:vertices [{:pos [-0.1 -0.1 0.0]}
                               {:pos [0.1 -0.1 0.0]}
                               {:pos [0.0 0.1 0.0]}]})
(def tri-mesh-layout {:buffer-layouts [{:attrib-layouts [{:name :pos
                                                          :type :float
                                                          :count 3}]}]
                      :element-type :triangles})

(def ^:private shader-program (e.t/shader-program-resolver [vert-shader frag-shader]))
(def ^:private tri-mesh (e.t/mesh-resolver tri-mesh-data tri-mesh-layout))

(defn render-entity
  [entity]
  (reify r/RenderOp
    (op-deps [_]
      {::tri tri-mesh
       ::shader-program shader-program})
    (apply-op! [_ {{::keys [tri shader-program]} ::r/resources}]
      (when (and tri shader-program)
        (sh/with-shader-program shader-program
          (apply sh/upload-uniform-float shader-program (c/sym->ident `obj-col) (::color entity))
          (apply sh/upload-uniform-float shader-program (c/sym->ident `obj-pos) (::d/position entity))
          (m/draw-mesh tri))))))

(defn draw-mesh
  [game-state]
  (->> (::ecs/entities game-state)
       vals
       (filter ::d/position)
       (map render-entity)))

(def init-game-state
  {::ecs/entities
   (let [[a b c] (repeatedly 3 ecs/next-entity-id)]
     {a {::d/dimensions 2
         ::d/position [0 0.5]
         ::d/mass 1
         ::d/velocity [0.25 0]
         ::color [1 0 0]}
      b {::d/dimensions 2
         ::d/position [-0.25 0]
         ::d/mass 10.2
         ::d/velocity [0 0]
         ::color [0 1 0]}
      c {::d/dimensions 2
         ::d/position [0.5 0]
         ::d/mass 1.15
         ::d/velocity [0 0.1]
         ::color [0 0 1]}})
   ::ecs/events []
   ::ecs/systems [#'e.t/ingest-input [#'gravity #'step-body]]
   ::r/systems [#'e.t/clear-screen #'draw-mesh]})

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (e.t/run-sim init-game-state e.t/init-render-state)
      (e.w/shutdown-window))
  (e.w/shutdown))
