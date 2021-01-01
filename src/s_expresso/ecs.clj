(ns s-expresso.ecs
  "Entity component system for simulating game logic."
  (:require
   [clojure.core.reducers :as r]
   [clojure.spec.alpha :as s])
  (:import
   (java.util UUID)))

(s/def ::scene (s/keys :req [::entities ::entities-to-spawn ::systems]))

(s/def ::entity (s/map-of keyword? any?))
(s/def ::entity-id uuid?)
(s/def ::entities (s/map-of ::entity-id ::entity))


(s/def ::entity-system (s/fspec :args (s/cat :scene ::scene
                                             :entity-id ::entity-id
                                             :entity ::entity
                                             :dt float?)
                                :ret (s/nilable ::entity)))
(s/def ::entity-systems (s/coll-of ::entity-system))
(s/def ::scene-system (s/fspec :args (s/cat :scene ::scene
                                            :dt float?)
                               :ret ::scene))
(s/def ::systems (s/coll-of (s/or :scene-system ::scene-system
                                  :entity-systems ::entity-systems)))

(s/def ::entities-to-spawn #(try (s/valid? (s/coll-of (s/tuple ::entity-id ::entity)) @%)
                                 (catch Throwable e)))

(defn make-scene
  [{:keys [entities systems]
    :or {entities {}
         systems []}}]
  {::entities entities
   ::entities-to-spawn (atom [])
   ::systems systems})
(s/fdef make-scene
  :args (s/cat :opts (s/keys :req-un [::entities ::systems]))
  :ret ::scene)

(defn step-scene
  "Runs all the systems over the `scene`, returning the new one.
  Any systems which are applied to the whole scene will be run on the thread
  this function is called from, but entity-specific systems may be run in
  parallel."
  [scene dt]
  (loop [remaining-systems (::systems scene)
         scene (update scene ::entities into (first (reset-vals! (::entities-to-spawn scene) [])))]
    (if (seq remaining-systems)
      (let [system (first remaining-systems)]
        (if (fn? system)
          (recur (rest remaining-systems)
                 (system scene dt))
          (letfn [(apply-systems [[entity-id entity]]
                    [entity-id (reduce #(%2 scene entity-id %1 dt) entity system)])]
            (recur (rest remaining-systems)
                   (update scene ::entities #(into {} (r/filter second (r/map apply-systems %))))))))
      scene)))
(s/fdef step-scene
  :args (s/cat :scene ::scene
               :dt float?)
  :ret ::scene)

(def next-entity-id #(UUID/randomUUID))

(defn spawn-entity!
  "Queues the entity for inclusion in the next frame of the scene."
  [scene entity]
  (let [uuid (next-entity-id)]
    (swap! (::entities-to-spawn scene) conj [uuid entity])
    uuid))
(s/fdef spawn-entity!
  :args (s/cat :scene ::scene
               :entity ::entity)
  :ret ::entity-id)

(defmacro defsystem
  {:arglists '([symbol [required-keys*] [scene entity-id entity dt] & body])}
  [symbol required-keys [_ _ entity _ :as bindings] & body]
  `(defn ~symbol
     ~bindings
     (if (every? #(contains? ~entity %) ~required-keys)
       (do ~@body)
       ~entity)))
(s/fdef defsystem
  :args (s/cat :symbol symbol?
               :required-keys (s/coll-of any? :kind vector?)
               :bindings (s/coll-of symbol? :kind vector? :count 4)
               :body (s/* any?)))
