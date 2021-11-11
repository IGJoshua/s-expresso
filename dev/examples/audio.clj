(ns examples.audio
  (:require
   [examples.window :as e.w]
   [examples.triangle :as e.t]
   [s-expresso.audio :as sfx]
   [s-expresso.ecs :as ecs]
   [s-expresso.render :as r]))

(def ^:private audio-source #(sfx/make-source))
(def ^:private audio-file #(sfx/make-sound "res/audio/Breaker.ogg"))

(defn sound
  [entity]
  (reify r/RenderOp
    (op-deps [_]
      {::audio-source audio-source
       ::audio-file audio-file})
    (apply-op! [_ {{::keys [audio-source audio-file]} ::r/resources}]
      (when (and audio-source audio-file)
        (when-not (sfx/source-playing? audio-source)
          (sfx/source-assign-sound audio-source audio-file)
          (sfx/source-loop audio-source)
          (sfx/source-play audio-source))
        (let [[x y] (::e.t/position entity)]
          (sfx/source-position audio-source x y 0))))))

(defn attach-sound
  [game-state]
  (->> (::ecs/entities game-state)
       (filter (comp ::e.t/position second))
       vals
       (map sound)))

(def init-game-state
  (update e.t/init-game-state ::r/systems conj #'attach-sound))

(defn start
  []
  (e.w/init)
  (sfx/init-openal)
  (sfx/bind-context-to-thread)
  (-> (e.w/start-window e.w/window-opts)
      (e.t/run-sim init-game-state e.t/init-render-state)
      (e.w/shutdown-window))
  (sfx/shutdown-openal)
  (e.w/shutdown))
