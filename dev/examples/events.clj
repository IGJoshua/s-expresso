(ns examples.events
  (:require
   [taoensso.timbre :as log]
   [s-expresso.ecs :as ecs :refer [defsystem]]))

(defsystem send-event [::target-entity]
  [_ entity-id {::keys [target-entity message] :as entity} _]
  (ecs/send-event! {::ecs/target target-entity
                    ::from entity-id})
  entity)

(defn print-events
  [{::ecs/keys [events entities] :as scene} dt]
  (doseq [event events]
    (when-let [entity (entities (::ecs/target event))]
      (log/debug "Message sent from:" (::from event)
                 "\nEntity message:" (::message entity))))
  (ecs/send-event! {::message "Tick!"})
  scene)

(def init-state (ecs/make-scene
                 {:entities {#uuid "2565634f-2d87-41b4-ac60-3dbeff307d9b"
                             {::target-entity #uuid "e9feafb0-6f1d-4394-9b52-082b0bd97109"
                              ::message "Message from entity one!"}
                             #uuid "e9feafb0-6f1d-4394-9b52-082b0bd97109"
                             {::target-entity #uuid "2565634f-2d87-41b4-ac60-3dbeff307d9b"
                              ::message "Message from entity two!"}}
                  :systems [[#'send-event] #'print-events]}))
(defonce state (atom init-state))
(comment

  (reset! state init-state)

  )

(defn step-state
  []
  (swap! state ecs/step-scene 0.001))
