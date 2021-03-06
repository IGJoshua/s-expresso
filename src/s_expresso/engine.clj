(ns s-expresso.engine
  "This namespace provides common entrypoints to compositions of the systems
  provided in s-expresso, including default game loops, etc."
  (:require
   [clojure.core.async :as a]
   [clojure.spec.alpha :as s]
   [s-expresso.ecs :as ecs]
   [s-expresso.render :as r]
   [s-expresso.window :as w])
  (:import
   (org.lwjgl.glfw GLFW)))

(def ^:dynamic ^:private *render-events-to-send*
  "Dynvar for the render events to be sent at the end of an entity system."
  nil)

(defn send-render-event!
  "Queues the event to run on the next render frame.
  This may only be called inside a system."
  [event]
  (when-not *render-events-to-send*
    (throw (ex-info "Attempted to send a render event outside of a binding." {:event event})))
  (set! *render-events-to-send* (conj *render-events-to-send* event))
  nil)
(s/fdef send-render-event!
  :args (s/cat :event ::event)
  :ret nil?)

(defonce
  ^{:private true
    :doc "A map containing the current game state and render events."}
  current-state
  (atom {}))

(defn- simulate
  "Simulates one tick of the ECS state per `dt` seconds.
  Assumes that [[w/time]] will not be set after this is called."
  [init-state dt]
  (loop [scene init-state
         next-frame (w/time)]
    ;; Spin waiting for the next frametime to start
    (let [t (- next-frame dt)]
      (while (pos? (- t (w/time)))))

    (let [scene (binding [*render-events-to-send* []]
                  (let [scene (ecs/step-scene scene dt)]
                    (swap! current-state
                           #(update (assoc % ::state
                                           (assoc scene ::time next-frame))
                                    ::events concat *render-events-to-send*))
                    scene))]
      (when (and scene (not (::should-close? scene)))
        (recur scene
               (let [next-frame (+ next-frame dt)
                     current-time (w/time)
                     amount-behind (- current-time next-frame)]
                 (if (>= amount-behind 0.1)
                   (do
                     (println "SIMULATION BEHIND! Dropping" amount-behind "seconds of real time.")
                     (+ current-time dt))
                   next-frame)))))))

(defn- render
  "Renders the [[current-state]].
  Returns the last game state and render state which were rendered. Assumes
  that [[w/time]] will not be set after this is called.

  Returns when `::should-close?` is set on the game state."
  [window init-state]
  (loop [render-state init-state]
    (w/poll-events)
    (let [[{::keys [state events]}] (swap-vals! current-state dissoc ::events)
          render-state (reduce (::render-event-handler state) render-state events)]
      (when state
        (r/step-renderer! render-state state))
      (w/swap-buffers window)
      (if (and state (not (::should-close? state)))
        (recur render-state)
        [state render-state]))))

(defn start-engine
  ""
  [window init-game-state init-render-state simulation-step]
  ;; Set the primary simulation thread to max priority and kick it off
  (a/thread
    (.setPriority (Thread/currentThread) Thread/MAX_PRIORITY)
    (simulate init-game-state simulation-step))

  ;; Set the render thread to max priority
  (.setPriority (Thread/currentThread) Thread/MAX_PRIORITY)

  (render window init-render-state))
