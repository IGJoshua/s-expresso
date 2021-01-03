(ns s-expresso.ecs
  "Entity component system for simulating game logic."
  (:require
   [clojure.core.reducers :as r]
   [clojure.spec.alpha :as s])
  (:import
   (java.util UUID)))

(s/def ::scene (s/keys :req [::entities ::systems]))

(s/def ::entity (s/map-of keyword? any?))
(s/def ::entity-id uuid?)
(s/def ::entities (s/map-of ::entity-id ::entity))


(s/def ::entity-system (s/fspec :args (s/cat :scene ::scene
                                             :entity-id ::entity-id
                                             :entity ::entity
                                             :dt float?)
                                :ret ::entities))
(s/def ::entity-systems (s/coll-of ::entity-system))
(s/def ::scene-system (s/fspec :args (s/cat :scene ::scene
                                            :dt float?)
                               :ret ::scene))
(s/def ::systems (s/coll-of (s/or :scene-system ::scene-system
                                  :entity-systems ::entity-systems)))

(defn make-scene
  [{:keys [entities systems]
    :or {entities {}
         systems []}}]
  {::entities entities
   ::systems systems})
(s/fdef make-scene
  :args (s/cat :opts (s/keys :req-un [::entities ::systems]))
  :ret ::scene)

(defn step-scene
  "Runs all the systems over the `scene`, returning the new one.
  Any systems which are applied to the whole scene will be run on the thread
  this function is called from, but entity-specific systems may be run in
  parallel.

  Entity specfic systems must return a map of entities, and if the entity the
  system was called on is not included in that map, no further systems are run
  on that entity."
  [scene dt]
  (loop [remaining-systems (::systems scene)
         scene scene]
    (if (seq remaining-systems)
      (let [system (first remaining-systems)]
        (if (fn? system)
          (recur (rest remaining-systems)
                 (system scene dt))
          (letfn [(apply-systems [[entity-id entity]]
                    (reduce (fn [{entity entity-id :as entities} system]
                              (let [new-entities (system scene entity-id entity dt)]
                                (if (new-entities entity-id)
                                  new-entities
                                  (reduced new-entities))))
                            {entity-id entity}
                            system))]
            (recur
             (rest remaining-systems)
             (update scene ::entities #(r/fold merge (r/filter second (r/map apply-systems %))))))))
      scene)))
(s/fdef step-scene
  :args (s/cat :scene ::scene
               :dt float?)
  :ret ::scene)

(def next-entity-id #(UUID/randomUUID))

(def ^:dynamic ^:private *entities-to-spawn* nil)

(defn spawn-entity!
  "Queues the entity for inclusion in the next frame of the scene.
  This may be called only from within a usage of [[defsystem]]."
  [entity]
  (let [uuid (next-entity-id)]
    (vswap! *entities-to-spawn* assoc uuid entity)
    uuid))
(s/fdef spawn-entity!
  :args (s/cat :entity ::entity)
  :ret ::entity-id)

(defmacro defsystem
  "Constructs a function to be used as a per-entity system.
  The body of the function will be run only if the entity has all of the
  required keys on its map, and the value returned from the function is the new
  entity.

  If nil is returned, the entity is deleted and no further systems are run.

  Within the body of the function, [[spawn-entity!]] may be used to queue
  another entity to be spawned."
  {:arglists '([symbol [required-keys*] [scene entity-id entity dt] & body])}
  [symbol required-keys [_ entity-id entity _ :as bindings] & body]
  `(defn ~symbol
     ~bindings
     (if (every? #(contains? ~entity %) ~required-keys)
       (binding [*entities-to-spawn* (volatile! {})]
         (let [new-entity# ~@body]
           (assoc @*entities-to-spawn* ~entity-id new-entity#)))
       {~entity-id ~entity})))
(s/fdef defsystem
  :args (s/cat :symbol symbol?
               :required-keys (s/coll-of any? :kind vector?)
               :bindings (s/coll-of symbol? :kind vector? :count 4)
               :body (s/* any?)))
