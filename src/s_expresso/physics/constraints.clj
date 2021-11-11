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
        magnitude (* (Math/abs ^double (- (m/magnitude to-target) distance))
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
  (+ (m/dot (or (::d/velocity body1) (m/zero-vector (::d/dimensions body1)))
            normal)
     (m/dot (or (::d/velocity body2) (m/zero-vector (::d/dimensions body2)))
            (m/negate normal))))
(s/fdef closing-velocity
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :normal m/vec?)
  :ret number?)

(defn- contact-impulse
  "Constructs an impulse vector to be applied to each body to resolve a contact."
  [body1 body2 contact restitution dt]
  (let [closing-velocity (or (::closing-velocity contact)
                             (closing-velocity body1 body2 (::c/normal contact)))]
    (if (or (<= closing-velocity 0)
            (every? #(= ##Inf (::d/mass % ##Inf)) [body1 body2]))
      [body1 body2]
      (let [accelleration-separation (m/dot (m/sub (or (::d/last-accelleration body1)
                                                       (m/zero-vector (::d/dimensions body1)))
                                                   (or (::d/last-accelleration body2)
                                                       (m/zero-vector (::d/dimensions body2))))
                                            (m/mul (::c/normal contact)
                                                   dt))
            separating-velocity (as-> (* closing-velocity restitution) vel
                                  (if (neg? accelleration-separation)
                                    (+ vel (* restitution accelleration-separation))
                                    (if (neg? vel) 0 vel)))
            delta-v (+ separating-velocity closing-velocity)
            body1-imass (/ (::d/mass body1 0))
            body2-imass (/ (::d/mass body2 0))
            total-mass (+ body1-imass body2-imass)
            impulse (/ delta-v total-mass)]
        [(m/mul (::c/normal contact)
                impulse (- body1-imass))
         (m/mul (::c/normal contact)
                impulse body2-imass)]))))
(s/fdef contact-impulse
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact
               :restitution number?
               :dt pos?)
  :ret m/vec?)

(defn- resolve-contact-impulse
  "Resolves velocity between two bodies for a given contact.
  Returns the modified bodies in the order they were passed."
  [body1 body2 contact restitution dt]
  (let [impulse (contact-impulse body1 body2 contact restitution dt)]
    [(d/add-impulse body1 impulse)
     (d/add-impulse body2 (m/negate impulse))]))
(s/fdef resolve-contact-impulse
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact
               :restitution number?
               :dt pos?)
  :ret (s/tuple ::d/body ::d/body))

(defn- contact-interpenetration
  "Returns the vectors to move the bodies by to resolve interpenetration.
  Each vector is scaled by the mass of each body in proportion to the total."
  [body1 body2 contact]
  (if (or (every? #(= ##Inf (::d/mass % ##Inf)) [body1 body2])
          (>= 0 (::c/penetration-depth contact)))
    [body1 body2]
    (let [penetration (m/mul (::c/normal contact) (::c/penetration-depth contact))
          total-mass (+ (::d/mass body1) (::d/mass body2))
          body1-proportion (double (/ (::d/mass body1 ##Inf) total-mass))
          body1-proportion (if (Double/isNaN body1-proportion) 0 body1-proportion)
          body2-proportion (- 1 body1-proportion)]
      [(m/mul penetration body1-proportion)
       (m/mul (m/negate penetration)
              body2-proportion)])))
(s/fdef contact-interpenetration
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact)
  :ret (s/tuple m/vec? m/vec?))

(defn- resolve-contact-interpenetration
  "Moves the two bodies to no longer be penetrating.
  Each body is moved an amount relative to its proportion of the total mass."
  [body1 body2 contact]
  (map #(update %1 ::d/position m/add %2)
       [body1 body2]
       (contact-interpenetration body1 body2 contact)))
(s/fdef resolve-contact-interpenetration
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact)
  :ret (s/tuple ::d/body ::d/body))

(defn resolve-contact
  "Resolves a contact between two bodies.
  Returns the modified bodies in the order they were passed."
  [body1 body2 contact restitution dt]
  (let [[body1 body2] (resolve-contact-impulse body1 body2 contact restitution dt)]
    (resolve-contact-interpenetration body1 body2 contact)))
(s/fdef resolve-contact
  :args (s/cat :body1 ::d/body
               :body2 ::d/body
               :contact ::c/contact
               :restitution number?
               :dt pos?)
  :ret (s/tuple ::d/body ::d/body))

(s/def ::bodies (s/tuple any? any?))

(defn- resolve-contacts-for-velocity
  "Applies velocity alterations to a map of bodies based on the contacts."
  [bodies contacts body-pair->restitution iterations dt]
  (loop [iterations iterations
         bodies bodies]
    (let [contacts (map #(assoc % ::closing-velocity
                                (let [body-keys (::bodies %)]
                                  (closing-velocity (get bodies (first body-keys))
                                                    (get bodies (second body-keys))
                                                    (::c/normal %))))
                        contacts)]
      (if (and (some (comp pos? ::closing-velocity) contacts)
               (pos? iterations))
        (recur
         (dec iterations)
         (loop [bodies bodies
                contacts (sort-by
                          ::closing-velocity
                          contacts)]
           (if (seq contacts)
             (let [[contact & contacts] contacts
                   [body1-key body2-key] (::bodies contact)
                   body1 (get bodies body1-key)
                   body2 (get bodies body2-key)
                   contact (assoc contact ::closing-velocity
                                  (closing-velocity body1 body2 (::c/normal contact)))
                   [body1-impulse body2-impulse]
                   (contact-impulse body1 body2 contact
                                    (body-pair->restitution body1 body2)
                                    dt)
                   bodies (assoc bodies
                                 body1-key (d/add-impulse body1 body1-impulse)
                                 body2-key (d/add-impulse body2 body2-impulse))]
               (recur
                bodies
                (sort-by
                 ::closing-velocity
                 contacts)))
             bodies)))
        bodies))))
(s/fdef resolve-contacts-for-velocity
  :args (s/cat :bodies (s/map-of any? ::d/body)
               :contacts (s/coll-of (s/keys :req [::c/contact ::bodies]))
               :body-pair->restitution (s/fspec :args (s/tuple ::d/body ::d/body)
                                                :ret number?)
               :iterations pos?
               :dt pos?)
  :ret (s/map-of any? ::d/body))

(defn- resolve-contacts-for-interpenetration
  "Moves `bodies` to no longer be interpenetrating.
  In complex situations, moving bodies to resolve interpenetration between one
  contact may cause a previous resolution to fail. No more than `iterations`
  attempts will be made to resolve complex interpenetrations, if they are not
  fully resolved after that many attempts, they will be left partially
  penetrating. This may cause collisions in some cases to feel 'soft'."
  [bodies contacts _body-pair->restitution _dt iterations]
  (loop [bodies bodies
         contacts contacts
         iterations iterations]
    (let [[bodies contacts]
          (loop [bodies bodies
                 contacts (sort-by ::c/penetration-depth contacts)]
            (if (seq contacts)
              (recur bodies (rest contacts))
              [bodies contacts]))]
      (if (pos? iterations)
        (recur bodies contacts (dec iterations))
        bodies))))
(s/fdef resolve-contacts-for-interpenetration
  :args (s/cat :bodies (s/map-of any? ::d/body)
               :contacts (s/coll-of (s/keys :req [::c/contact ::bodies]))
               :body-pair->restitution (s/fspec :args (s/tuple ::d/body ::d/body)
                                                :ret number?)
               :dt pos?
               :iterations pos?)
  :ret (s/map-of any? ::d/body))

(defn resolve-contacts
  "Resolves a set of `contacts` for a map of `bodies`.
  Interpenetrations will attempt to be resolved for several `iterations` (default
  of twice the number of contacts). If interpenetrations cannot be fully
  resolved in this number of iterations, "
  ([bodies contacts body-pair->restitution dt]
   (resolve-contacts bodies contacts body-pair->restitution dt (* 2 (count contacts))))
  ([bodies contacts body-pair->restitution dt iterations]
   (let [bodies (resolve-contacts-for-velocity bodies contacts body-pair->restitution iterations dt)]
     (resolve-contacts-for-interpenetration bodies contacts body-pair->restitution dt iterations))))
(s/fdef resolve-contacts
  :args (s/cat :bodies (s/map-of any? ::d/body)
               :contacts (s/coll-of (s/keys :req [::c/contact ::bodies]))
               :body-pair->restitution (s/fspec :args (s/tuple ::d/body ::d/body)
                                                :ret number?)
               :dt number?
               :iterations (s/? pos?))
  :ret (s/map-of any? ::d/body))
