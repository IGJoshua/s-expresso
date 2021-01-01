(ns s-expresso.physics.constraints
  "Calculations for forces, displacements, and impulses needed for upholding constraints."
  (:require
   [clojure.core.matrix :as m]
   [clojure.spec.alpha :as s]))

(defn spring-force
  "Constructs a force vector from the `origin` to the given distance from `target`."
  [origin target distance spring-constant]
  (let [to-target (m/sub target origin)
        magnitude (* (Math/abs (- (m/magnitude to-target) distance))
                     spring-constant)]
    (m/mul (m/normalise to-target)
           magnitude)))
(s/fdef spring-force
  :args (s/cat :origin m/vec?
               :target m/vec?
               :distance number?
               :spring-constant number?)
  :ret m/vec?)
