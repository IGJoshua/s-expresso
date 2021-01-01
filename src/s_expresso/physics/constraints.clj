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

(defn gravitational-force
  "Constructs a force vector for the entity in the direction of the body.

  This is used for orbital simulations and other gravity effects, not for
  applying a constant downward velocity to objects."
  [entity-pos entity-mass body-pos body-mass gravity-constant]
  (let [to-body (m/sub body-pos entity-pos)]
    (m/mul (m/normalise to-body)
           (/ (* gravity-constant entity-mass body-mass)
              (m/magnitude-squared to-body)))))
(s/fdef gravitational-force
  :args (s/cat :entity-pos m/vec?
               :entity-mass pos?
               :body-pos m/vec?
               :body-mass pos?
               :gravity-constant pos?)
  :ret m/vec?)
