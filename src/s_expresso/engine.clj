(ns s-expresso.engine
  "This namespace provides common entrypoints to compositions of the systems
  provided in s-expresso, including default game loops, etc."
  (:require
   [clojure.core.async :as a]
   [clojure.spec.alpha :as s]
   [clojure.tools.logging :as log]
   [s-expresso.ecs :as ecs]
   [s-expresso.render :as r]
   [s-expresso.window :as w])
  (:import
   (s_expresso.window Window)))

(s/def ::should-close? boolean?)
(s/def ::time float?)
(s/def ::game-state (s/merge ::ecs/scene
                             ::r/game-state
                             (s/keys :req [::event-handler]
                                     :opt [::events ::should-close? ::time])))

(s/def ::step number?)
(s/def ::render-state (s/merge ::r/render-state
                               (s/keys :opt [::step])))

(s/def ::event any?)
(s/def ::events (s/coll-of ::event))
(s/def ::event-handler (s/fspec :args (s/cat :render-state ::render-state
                                             :event ::event)
                                :ret ::render-state))

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
    :doc "An atomic vector of game states which have been simulated."}
  simulated-states
  (atom []))

(defn- simulate
  "Simulates one tick of the ECS state per `dt` seconds.

  Assumes that [[w/time]] will not be set after this is called.

  As each state is simulated, it will be accumulated in [[simulated-states]]."
  [init-state dt close-ch]
  (loop [scene init-state
         next-frame (w/time)]
    ;; Allow the engine to kill this process early
    (when-not (a/poll! close-ch)

      ;; Spin waiting for the next frametime to start
      ;; TODO(Joshua): Maybe make this sleep for long wait times?
      (let [t (- next-frame dt)]       ; Set t to one dt before the frametime
        (while (pos? (- t (w/time))))) ; Wait until t

      (let [scene (binding [*render-events-to-send* []]
                    (let [scene (ecs/step-scene scene dt)]
                      ;; TODO(Joshua): Make this wait for the renderer to catch
                      ;; up if there's too many
                      (swap! simulated-states
                             conj (assoc scene ::time next-frame
                                         ::events *render-events-to-send*))
                      scene))]
        (when (and scene (not (get scene ::should-close?)))
          (recur scene
                 (let [next-frame (+ next-frame dt)
                       current-time (w/time)
                       amount-behind (- current-time next-frame)]
                   (if (>= amount-behind 0.1)
                     (do
                       (log/warn (str "SIMULATION BEHIND! Dropping " amount-behind " seconds of real time."))
                       (+ current-time dt))
                     next-frame))))))))

(defn- render
  "Starts a render loop to render states from [[simulated-states]].

  Returns the last game state and render state which were rendered. Assumes
  that [[w/time]] will not be set after this is called.

  Returns when `::should-close?` is set on the game state."
  [window init-state]
  (loop [render-state init-state
         next-vblank (when (::step render-state)
                       (- (w/time) (::step render-state)))]
    (w/poll-events)

    ;; Spin waiting for being within one vblank of the next one
    ;; TODO(Joshua): Maybe make this sleep for long wait times?
    ;; TODO(Joshua): Determine if this is needed. For most cases I think it
    ;; shouldn't be.
    (when next-vblank
      (let [t (+ next-vblank (::step render-state))]
        (when (pos? (- t (w/time)))
          (log/warn "Not within 1 timestep of vblank")
          (while (pos? (- t (w/time)))))))

    (let [last-state (volatile! nil)
          next-state (volatile! nil)
          events (volatile! nil)
          step (get render-state ::step)
          _ (swap-vals! simulated-states
                        (fn [ss]
                          (let [[past-states future-states] (if step
                                                              (split-with #(> next-vblank (get % ::time)) ss)
                                                              (split-at (dec (count ss)) ss))
                                prev-state (last past-states)]
                            (when step
                              (vreset! last-state prev-state))
                            (vreset! next-state (first future-states))
                            (vreset! events (concat (apply concat (keep #(get % ::events) past-states))
                                                    (get (first future-states) ::events)))
                            (vec (cond->> (cons (dissoc (first future-states) ::events)
                                                (rest future-states))
                                   prev-state (cons (dissoc prev-state ::events)))))))
          render-state (reduce (get @next-state ::event-handler) render-state @events)
          last-time (get ::time @last-state)
          render-state (if (and @next-state @last-state)
                         (r/step-renderer! render-state @next-state @last-state
                                           (/ (- next-vblank last-time)
                                              (- (get @next-state ::time) last-time)))
                         (when @next-state
                           (r/step-renderer! render-state @next-state)))]
      (w/swap-buffers window)
      (if (and (not (get @next-state ::should-close?)) ; don't want to close
               (or @next-state (not @last-state)))     ; and this isn't a nil state after the first
        (recur render-state
               (when step
                 (let [current-frame (- (w/time) step)
                       frames-behind (Math/floor (/ (- current-frame next-vblank) step))]
                   (+ (or next-vblank current-frame)
                      (if (< frames-behind 2)
                        step
                        (* step frames-behind))))))
        [@next-state render-state]))))

(defn start-engine
  "Starts a game loop.

  This is based on the [[w/time]] value increasing with time. When
  `::should-close?` is set on the game state, the simulation will complete that
  step, render it, and then close. The passed `window` will have its framebuffer
  swapped on each render, and events will be polled on each frame as well.

  When an `::events` key is included on a game state, those events are
  guaranteed to be processed before that state or any later one is rendered. The
  events are processed through the `::event-handler` provided on the render
  state.

  If `::step` is set on `init-render-state`, it will be used as the vsync step.
  This can be updated by sending `::events`."
  [window init-game-state init-render-state simulation-step]

  ;; Reset state from previous runs
  (reset! simulated-states [])

  (let [close-simulation (a/chan 1)
        simulation-thread
        ;; Set the primary simulation thread to max priority and kick it off
        (doto (Thread.
               #(do (.setPriority (Thread/currentThread) Thread/MAX_PRIORITY)
                    (simulate init-game-state simulation-step close-simulation)))
          (.start))
        ;; Set the render thread to max priority
        old-priority (.getPriority (Thread/currentThread))
        _ (.setPriority (Thread/currentThread) Thread/MAX_PRIORITY)]
    (try (render window init-render-state)
         (finally
           ;; Send a kill signal to the simulation and wait for it
           (a/put! close-simulation true)
           (.join simulation-thread)
           ;; Return this thread to the old priority and return
           (.setPriority (Thread/currentThread) old-priority)))))
(s/fdef start-engine
  :args (s/cat :window (partial instance? Window)
               :init-game-state ::game-state
               :init-render-state ::render-state
               :simulation-step number?)
  :ret (s/cat :game-state ::game-state
              :render-state ::render-state))
