(ns examples.events
  (:require
   [taoensso.timbre :as log]
   [s-expresso.ecs :as ecs :refer [defsystem]]))

(defsystem send-event [::target-entity]
  [_ entity-id {::keys [target-entity] :as entity} _]
  (ecs/send-event! {::ecs/event-type :message-pass
                    ::ecs/target target-entity
                    ::ecs/source entity-id})
  entity)

(defn print-events
  [{::ecs/keys [events entities] :as scene} dt]
  (doseq [event events]
    (if-let [entity (entities (::ecs/target event))]
      (log/debug "Message sent from:" (::ecs/source event)
                 "\nEntity message:" (::message entity))
      (log/debug "Message sent from:" (::ecs/source event)
                 "\nEntity message:" (::message event))))
  (ecs/send-event! {::ecs/event-type :global-message
                    ::ecs/source :print-events
                    ::message "Tick!"})
  scene)

(def init-state {::ecs/entities {#uuid "2565634f-2d87-41b4-ac60-3dbeff307d9b"
                                 {::target-entity #uuid "e9feafb0-6f1d-4394-9b52-082b0bd97109"
                                  ::message "Message from entity one!"}
                                 #uuid "e9feafb0-6f1d-4394-9b52-082b0bd97109"
                                 {::target-entity #uuid "2565634f-2d87-41b4-ac60-3dbeff307d9b"
                                  ::message "Message from entity two!"}}
                 ::ecs/systems [[#'send-event] #'print-events]})
(defonce state (atom init-state))
(comment

  (reset! state init-state)

  )

(defn step-state
  []
  (swap! state ecs/step-scene 0.001))

(defn start
  []
  (loop []
    (step-state)
    (Thread/sleep 1000)
    (recur)))
