(ns s-expresso.physics.constraints
  "Calculations for forces, displacements, and impulses needed for upholding constraints."
  (:require
   [clojure.core.matrix :as m]
   [clojure.spec.alpha :as s]
   [s-expresso.physics.collision :as c]
   [s-expresso.physics.dynamics :as d]))

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

(defn- closing-velocity
  "Calculates the closing velocity between two bodies.
  Returns a signed scalar value of the velocity along the normal. Positive means
  the objects are closing, negative they are separating.

  `normal` must be a unit vector in the direction from `body1` to `body2`"
  [body1 body2 normal]
  (+ (m/dot (::d/velocity body1 (m/zero-vector (::d/dimensions body1)))
            normal)
     (m/dot (::d/velocity body2 (m/zero-vector (::d/dimensions body2)))
            (m/negate normal))))
(s/fdef closing-velocity
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :normal m/vec?)
  :ret number?)

(defn- resolve-contact-impulse
  "Resolves velocity between two bodies for a given contact.
  Returns the modified bodies in the order they were passed."
  [body1 body2 contact restitution dt]
  (let [closing-velocity (closing-velocity body1 body2 (::c/normal contact))]
    (if (or (zero? closing-velocity)
            (not (every? #(= ##Inf (::d/mass % ##Inf)) [body1 body2])))
      [body1 body2]
      (let [accelleration-separation (m/dot (m/sub (::d/last-accelleration
                                                    body1 (m/zero-vector (::d/dimensions body1)))
                                                   (::d/last-accelleration
                                                    body2 (m/zero-vector (::d/dimensions body2))))
                                            (m/mul (::c/normal contact)
                                                   dt))
            separating-velocity (as-> (* closing-velocity restitution) vel
                                  (if (neg? accelleration-separation)
                                    (+ vel (* restitution accelleration-separation))
                                    vel)
                                  (if (neg? vel) 0 vel))
            delta-v (+ separating-velocity closing-velocity)
            total-mass (+ (::d/mass body1 0) (::d/mass body2 0))
            impulse (m/mul (::c/normal contact)
                           (* delta-v total-mass))]
        [(d/add-impulse body1 impulse)
         (d/add-impulse body2 (m/negate impulse))]))))
(s/fdef resolve-contact-impulse
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact
               :restitution number?)
  :ret (s/tuple ::d/body ::d/body))

(defn resolve-contact-interpenetration
  "Moves the two bodies to no longer be penetrating.
  Each body is moved an amount relative to its proportion of the total mass."
  [body1 body2 contact]
  (if (or (every? #(= ##Inf (::d/mass % ##Inf)) [body1 body2])
          (>= 0 (::c/penetration-depth contact)))
    [body1 body2]
    (let [penetration (m/mul (::c/normal contact) (::c/penetration-depth contact))
          total-mass (+ (::d/mass body1) (::d/mass body2))
          body1-proportion (double (/ (::d/mass body1 ##Inf) total-mass))
          body1-proportion (if (Double/isNaN body1-proportion) 0 body1-proportion)
          body2-proportion (- 1 body1-proportion)]
      [(update body1 ::d/position m/add (m/mul penetration body1-proportion))
       (update body2 ::d/position m/add (m/mul (m/negate penetration)
                                               body1-proportion))])))
(s/fdef resolve-contact-interpenetration
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact)
  :ret (s/tuple ::d/body ::d/body))

(defn resolve-contact
  "Resolves a contact between two bodies.
  Returns the modified bodies in the order they were passed."
  [body1 body2 contact restitution dt]
  (let [[body1 body2] (resolve-contact-impulse body1 body2 contact restitution)]
    (resolve-contact-interpenetration body1 body2 contact)))
(s/fdef resolve-contact
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact
               :restitution number?
               :dt number?)
  :ret (s/tuple ::d/body ::d/body))
