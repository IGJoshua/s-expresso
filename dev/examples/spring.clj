(ns examples.spring
  (:require
   [clojure.core.matrix :as mat]
   [examples.gravity :as e.g]
   [examples.triangle :as e.t]
   [examples.window :as e.w]
   [s-expresso.render :as r]
   [s-expresso.ecs :as ecs :refer [defsystem]]
   [s-expresso.physics.dynamics :as d]
   [s-expresso.physics.constraints :as constraint]))

(def gravity-vector (mat/array [0 -9.81]))

(defsystem gravity [::d/dimensions ::d/mass]
  [_ _ entity _]
  (d/add-force entity (mat/mul gravity-vector (::d/mass entity))))

(defsystem damping [::d/velocity]
  [_ _ entity _]
  (update entity ::d/velocity mat/mul 0.99)
  entity)

(defsystem spring-constraint [::constraints ::d/dimensions ::d/position]
  [scene _ entity _]
  (reduce
   (fn [body constraint]
     (d/add-force
      body
      (let [target (:target constraint)
            target-pos (if (uuid? target)
                         (::d/position ((::ecs/entities scene) target))
                         target)]
        (constraint/spring-force (::d/position body)
                                 target-pos
                                 (:distance constraint)
                                 (:constant constraint)))))
   entity
   (::constraints entity)))

(defsystem step-body [::d/dimensions ::d/position ::d/mass ::d/velocity]
  [_ _ entity dt]
  (d/step-body entity dt))

(def init-game-state
  {::ecs/entities
   (let [[a b c] (repeatedly 3 ecs/next-entity-id)]
     {a {::d/dimensions 2
         ::d/position [0 0.7]
         ::e.g/color [1 0 0]}
      b {::d/dimensions 2
         ::d/position [0.3 0.5]
         ::d/mass 2.0
         ::d/velocity [0 0]
         ::e.g/color [0 1 0]
         ::constraints [{:constant 100
                         :distance 0.2
                         :target a}
                        {:constant 30
                         :distance 0.3
                         :target c}]}
      c {::d/dimensions 2
         ::d/position [0 0.2]
         ::d/mass 1.0
         ::d/velocity [0 0]
         ::e.g/color [0 0 1]
         ::constraints [{:constant 30
                         :distance 0.3
                         :target b}]}})
   ::ecs/events []
   ::ecs/systems [#'e.t/ingest-input [#'gravity #'spring-constraint #'step-body #'damping]]
   ::r/systems [#'e.t/clear-screen #'e.g/draw-mesh]})

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (e.t/run-sim init-game-state e.t/init-render-state)
      (e.w/shutdown-window))
  (e.w/shutdown))
