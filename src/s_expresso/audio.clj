(ns s-expresso.audio
  "Functions for creating an OpenAL context and playing sounds."
  (:import
   (org.lwjgl.openal AL AL11 ALC ALC11)))

(def alc-device (atom nil))
(def alc-capabilities (atom nil))
(def al-capabilities (atom nil))
(def al-context (atom nil))

(defn init-openal
  []
  (ALC/create)
  (let [device-specifier (ALC11/alcGetString 0 ALC11/ALC_DEFAULT_DEVICE_SPECIFIER)
        device (ALC11/alcOpenDevice device-specifier)]
    (when-not (zero? device)
      (let [alc-caps (ALC/createCapabilities device)
            al-ctx (ALC11/alcCreateContext device (int-array 0))]
        (reset! alc-device device)
        (reset! alc-capabilities alc-caps)
        (reset! al-context al-ctx)
        (when-not (zero? al-ctx)
          (ALC11/alcMakeContextCurrent al-ctx)
          (ALC11/alcProcessContext al-ctx))
        (let [al-caps (AL/createCapabilities alc-caps)]
          (reset! al-capabilities al-caps)
          (when al-caps
            (AL/setCurrentProcess al-caps))))))
  nil)

(defn bind-context-to-thread
  []
  (when-let [caps @al-capabilities]
    (AL/setCurrentThread caps)))

(defn shutdown-openal
  []
  (when-let [ctx @al-context]
    (when-not (zero? ctx)
      (ALC11/alcDestroyContext ctx)))
  (ALC/destroy))

(defn suspend-context
  []
  (when-let [ctx @al-context]
    (when-not (zero? ctx)
      (ALC11/alcSuspendContext ctx))))

(defn resume-context
  []
  (when-let [ctx @al-context]
    (when-not (zero? ctx)
      (ALC11/alcProcessContext ctx))))
