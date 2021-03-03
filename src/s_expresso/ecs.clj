(ns s-expresso.ecs
  "Entity component system for simulating game logic."
  (:require
   [clojure.core.reducers :as r]
   [clojure.spec.alpha :as s])
  (:import
   (java.util UUID)))

(s/def ::scene (s/keys :req [::entities ::systems ::events]))

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

(s/def ::target ::entity-id)
(s/def ::source (s/or :entity ::entity-id
                      :named-source keyword?))
(s/def ::event-type keyword?)
(s/def ::event (s/keys :req [::event-type ::source]
                       :opt [::target]))
(s/def ::events (s/coll-of ::event))

(def ^:dynamic *events-to-send*
  "Dynvar for the events to be sent at the end of an entity system."
  nil)

(defn send-event!
  "Queues the event for inclusion in the next frame of the scene.
  This may be called only from within a system."
  [event]
  (when-not *events-to-send*
    (throw (ex-info "Attempted to send an event outside of a system." {:event event})))
  (set! *events-to-send* (conj *events-to-send* event))
  nil)
(s/fdef send-event!
  :args (s/cat :event ::event)
  :ret nil?)

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
        (if-not (vector? system)
          (recur (rest remaining-systems)
                 (binding [*events-to-send* []]
                   (update (system scene dt)
                           ::events-to-send concat *events-to-send*)))
          (letfn [(apply-systems [entity-id entity]
                    (reduce (fn [{entity entity-id :as entities} system]
                              (let [new-entities (system scene entity-id entity dt)]
                                (if (new-entities entity-id)
                                  new-entities
                                  (reduced new-entities))))
                            {entity-id entity}
                            system))]
            (recur
             (rest remaining-systems)
             (let [[scene events]
                   (binding [*events-to-send* []]
                     [(update scene ::entities #(r/fold merge (r/map apply-systems %)))
                      (concat (::events-to-send scene) *events-to-send*)])]
               (assoc scene ::events-to-send events))))))
      (assoc (dissoc scene ::events-to-send)
             ::events (::events-to-send scene)))))
(s/fdef step-scene
  :args (s/cat :scene ::scene
               :dt float?)
  :ret ::scene)

(def next-entity-id #(UUID/randomUUID))

(def ^:dynamic *entities-to-spawn*
  "Dynvar for the entities to be spawned at the end of a [[defsystem]] function."
  nil)

(defn spawn-entity!
  "Queues the entity for inclusion in the next frame of the scene.
  This may be called only from within a usage of [[defsystem]]."
  [entity]
  (when-not *entities-to-spawn*
    (ex-info "Attempted to spawn an entity outside of a per-entity system." {:entity entity}))
  (let [uuid (next-entity-id)]
    (set! *entities-to-spawn* (assoc *entities-to-spawn* uuid entity))
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
  [symbol required-keys bindings & body]
  (let [gen-bindings (repeatedly 4 gensym)
        [_ entity-id entity _] gen-bindings]
    `(defn ~symbol
       [~@gen-bindings]
       (let [~bindings [~@gen-bindings]]
         (if (every? #(contains? ~entity %) ~required-keys)
           (binding [*entities-to-spawn* {}]
             (let [new-entity# (do ~@body)]
               (assoc *entities-to-spawn* ~entity-id new-entity#)))
           {~entity-id ~entity})))))
(s/fdef defsystem
  :args (s/cat :symbol symbol?
               :required-keys (s/coll-of any? :kind vector?)
               :bindings (s/coll-of (s/or :symbol symbol?
                                          :destructured-map map?)
                                    :kind vector?
                                    :count 4)
               :body (s/* any?)))
