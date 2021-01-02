(ns s-expresso.physics.dynamics
  (:require
   [clojure.core.matrix :as m]
   [clojure.spec.alpha :as s]))

;; Linear dynamics
(s/def ::position m/vec?)
(s/def ::mass pos?)
(s/def ::velocity m/vec?)
(s/def ::net-force m/vec?)
(s/def ::last-accelleration m/vec?)

;; Angular Dynamics
(s/def ::moment-of-inertia (s/or :two-dimensional
                                 float?
                                 :three-dimensional
                                 (s/and m/array?
                                        #(= (m/shape %) [3 3]))))
(s/def ::angular-velocity (s/or :two-dimensional float?
                                :three-dimensional m/vec?))
(s/def ::orientation (s/or :two-dimensional float?
                           :three-dimensional m/vec?))

(s/def ::dimensions nat-int?)
(s/def ::body (s/and (s/keys :req [::dimensions
                                   ::position ::mass ::velocity
                                   ::orientation ::moment-of-inertia ::angular-velocity]
                             :opt [::net-force ::last-accelleration])
                     (comp (partial apply =)
                           (juxt (comp count ::position)
                                 (comp count ::velocity)))))

(defn accelleration
  [body]
  (m/div (::net-force body)
         (::mass body)))
(s/fdef accelleration
  :args (s/cat :body ::body)
  :ret m/vec?)

(defn step-body
  [body dt]
  (let [accelleration (accelleration body)
        new-velocity (m/add (::velocity body) (m/mul accelleration dt))]
    (dissoc
     (assoc body
            ::velocity new-velocity
            ::position (m/add (::position body) (m/mul new-velocity dt)))
     ::net-force)))
(s/fdef step-body
  :args (s/cat :body ::body
               :dt float?)
  :ret ::body)

(defn add-force
  [body force]
  (update body ::net-force (fnil m/add (m/zero-vector (::dimensions body))) force))
(s/fdef add-force
  :args (s/cat :body ::body
               :force m/vec?)
  :ret ::body)

(defn add-impulse
  [body impulse]
  (update body ::velocity m/add (m/mul impulse (/ (::mass body)))))
(s/fdef add-impulse
  :args (s/cat :body ::body
               :impulse m/vec?)
  :ret ::body)
