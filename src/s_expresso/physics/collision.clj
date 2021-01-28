(ns s-expresso.physics.collision
  (:require
   [clojure.core.matrix :as m]
   [clojure.spec.alpha :as s]))

(defprotocol Hull
  "Defines a way to find a point on the object furthest in a given direction."
  :extend-via-metadata true
  (point-in-direction
    [_ dir]
    "Returns a point which is maximal in its distance along the direction vector."))

(defn- minkowski-hull-point
  "Finds the closest point on the hull of the minkowski sum given a direction.
  Requires that [[Hull]] be implemented for `a` and `b`."
  [a b dir]
  (m/sub (point-in-direction a dir)
           (point-in-direction b (m/negate dir))))
(s/fdef minkowski-hull-point
  :args (s/cat :a (partial satisfies? Hull)
               :b (partial satisfies? Hull)
               :dir m/vec?)
  :ret m/vec?)

(defn- same-dir?
  "Returns true when a and b point in the same direction.
  Both `a` and `b` must be vectors."
  [a b]
  (> (m/dot a b) 0))
(s/fdef same-dir?
  :args (s/cat :a m/vec?
               :b m/vec?)
  :ret boolean?)

(defn- evaluate-line
  "Reduces a line to the next simplex and direction."
  [line dir]
  (let [[a b] line
        ab (m/sub b a)
        ao (m/negate a)]
    (if (same-dir? ab ao)
      [line (m/cross (m/cross ab ao) ab)]
      [(list a) ao])))
(s/fdef evaluate-line
  :args (s/cat :line (s/and (s/coll-of m/vec?)
                            #(= 2 (count %)))
               :dir m/vec?)
  :ret (s/cat :simplex (s/coll-of m/vec?)
              :dir m/vec?))

(defn- evaluate-triangle
  "Reduces a triangle to the next simplex and direction."
  [triangle dir]
  (let [[a b c] triangle
        ab (m/sub b a)
        ac (m/sub c a)
        ao (m/negate a)
        abc (m/cross ab ac)]
    (if (same-dir? (m/cross abc ac) ao)
      (if (same-dir? ac ao)
        [(list a c) (m/cross (m/cross ac ao) ac)]
        (evaluate-line (list a b) dir))
      (if (same-dir? (m/cross ab abc) ao)
        (evaluate-line (list a b) dir)
        (if (same-dir? abc ao)
          [triangle abc]
          [(list a c b) (m/negate abc)])))))
(s/fdef evaluate-triangle
  :args (s/cat :triangle (s/and (s/coll-of m/vec?)
                                #(= 3 (count %)))
               :dir m/vec?)
  :ret (s/cat :simplex (s/coll-of m/vec?)
              :dir m/vec?))

(defn- evaluate-tetrahedron
  "Reduces a tetrahedron to the next simplex and direction."
  [tetrahedron dir]
  (let [[a b c d] tetrahedron
        ab (m/sub b a)
        ac (m/sub c a)
        ad (m/sub d a)
        ao (m/negate a)
        abc (m/cross ab ac)
        acd (m/cross ac ad)
        adb (m/cross ad ab)]
    (cond
      (same-dir? abc ao) (evaluate-triangle (list a b c) dir)
      (same-dir? acd ao) (evaluate-triangle (list a c d) dir)
      (same-dir? adb ao) (evaluate-triangle (list a d b) dir)
      :otherwise [tetrahedron nil])))
(s/fdef evaluate-tetrahedron
  :args (s/cat :tetrahedron (s/and (s/coll-of m/vec?)
                                   #(= 4 (count %)))
               :dir m/vec?)
  :ret (s/cat :simplex (s/coll-of m/vec?)
              :dir (s/nilable m/vec?)))

(defn- evaluate-simplex
  "Reduce the simplex to points near the origin and return a new direction to search.
  If the input is a tetrahedron and the origin is contained by the simplex, the
  direction will be nil."
  [simplex dir]
  (case (count simplex)
    2 (evaluate-line simplex dir)
    3 (evaluate-triangle simplex dir)
    4 (evaluate-tetrahedron simplex dir)))
(s/fdef evaluate-simplex
  :args (s/cat :simplex (s/and (s/coll-of m/vec?)
                               #(<= 2 (count %) 4))
               :dir m/vec?)
  :ret (s/cat :simplex (s/coll-of m/vec?)
              :dir (s/nilable m/vec?)))

(defn collision-simplex
  "Returns a simplex in the Minkowski sum of two objects containing the origin.
  If the origin is not contained within the sum, nil is returned.

  Each of the two objects must be implementations of the [[Hull]] protocol, and
  this is the only interface requried for interacting with them.

  This is implemented using a form of the GJK algorithm."
  [a b]
  (loop [simplex (list (minkowski-hull-point a b (m/array [1 0 0])))
         dir (m/negate (first simplex))]
    (let [support (minkowski-hull-point a b dir)]
      (when (> (m/dot support dir) 0)
        (let [[simplex dir] (evaluate-simplex (conj simplex support) dir)]
          (if (= 4 (count simplex))
            simplex
            (recur simplex dir)))))))
(s/fdef collision-simplex
  :args (s/cat :a (partial satisfies? Hull)
               :b (partial satisfies? Hull))
  :ret (s/coll-of m/vec?))

(defn overlapping?
  "Returns true if the two objects are overlapping, false otherwise.
  Both `a` and `b` must implement [[Hull]]."
  [a b]
  (some? (collision-simplex a b)))
(s/fdef overlapping?
  :args (s/cat :a (partial satisfies? Hull)
               :b (partial satisfies? Hull))
  :ret boolean?)

(defn sphere-support
  "Returns the furthest point in a given `direction` around a sphere.
  This function is designed to be used in an implementation of [[Hull]] for
  spheres.

  This constructs the point from the definition of a sphere and will be
  incredibly fast."
  [center radius direction]
  (m/add center
         (m/mul (m/normalise direction)
                radius)))
(s/fdef sphere-support
  :args (s/cat :center m/vec?
               :radius pos?
               :direction m/vec?)
  :ret m/vec?)

(defn points-support
  "Returns the furthest point in a given `direction` among a sequence of points.
  This function is designed to be used in an implementation of [[Hull]] for
  convex hull meshes and other structures defined with a small set of vertices.

  This uses a search through the points, and as such is linear in complexity."
  [points direction]
  (second
   (let [[point & points] points
         direction (m/normalise direction)]
     (reduce (fn [[max-dot max-point :as acc] new-point]
               (let [new-dot (m/dot new-point direction)]
                 (if (> new-dot max-dot)
                   [new-dot new-point]
                   acc)))
             [(m/dot point direction) point]
             points))))
(s/fdef points-support
  :args (s/cat :points (s/coll-of m/vec?)
               :direction m/vec?)
  :ret m/vec?)

(defn aabb-support
  "Returns the furthest point in a given `direction` on the surface of an aabb.
  This function is designed to be used in an implementation of [[Hull]] for axis
  aligned bounding boxes."
  [center bounds direction]
  (m/add center (m/mul bounds (m/emap #(Math/signum ^double %) direction))))
(s/fdef aabb-support
  :args (s/cat :center m/vec?
               :bounds m/vec?
               :direction m/vec?)
  :ret m/vec?)

(s/def ::penetration-depth number?)
(s/def ::normal m/vec?)
(s/def ::contact (s/keys :req [::normal ::penetration-depth]))
