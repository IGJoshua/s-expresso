(ns s-expresso.audio
  "Functions for creating an OpenAL context and playing sounds."
  (:require
   [s-expresso.memory :as mem :refer [with-stack-allocator alloc-bytes put put-seq]]
   [s-expresso.resource :refer [Resource]])
  (:import
   (org.lwjgl.openal AL AL11 ALC ALC11)
   (org.lwjgl.stb STBVorbis)
   (org.lwjgl.system.libc LibCStdlib)))

(defonce
  ^{:doc "An atom containing device handle for OpenAL."}
  alc-device
  (atom nil))
(defonce
  ^{:doc "An atom containing the [[org.lwjgl.openal.ALCCapabilities]] for opening OpenAL contexts."}
  alc-capabilities
  (atom nil))
(defonce
  ^{:doc "An atom containing the [[org.lwjgl.openal.ALCapabilities]] for OpenAL."}
  al-capabilities
  (atom nil))
(defonce
  ^{:doc "An atom containing the context handle for OpenAL."}
  al-context
  (atom nil))

(defn init-openal
  "Initializes OpenAL for use on the system."
  ([] (init-openal {}))
  ([opts]
   (try (ALC/create)
        (catch IllegalStateException e))
   (let [{:keys []
          :or {}} opts
         device-specifier (ALC11/alcGetString 0 ALC11/ALC_DEFAULT_DEVICE_SPECIFIER)
         device (ALC11/alcOpenDevice device-specifier)]
     (when-not (zero? device)
       (let [alc-caps (ALC/createCapabilities device)
             al-ctx (ALC11/alcCreateContext device (int-array [0]))]
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
   nil))

(defn bind-context-to-thread
  "Sets the current thread to be the one for OpenAL's context to use."
  []
  (when-let [caps @al-capabilities]
    (AL/setCurrentThread caps))
  (when-let [al-ctx @al-context]
    (when-not (zero? al-ctx)
      (ALC11/alcMakeContextCurrent al-ctx))))

(defn shutdown-openal
  "Shuts down OpenAL in the current system."
  []
  (when-let [ctx @al-context]
    (when-not (zero? ctx)
      (ALC11/alcDestroyContext ctx)))
  (when-let [device @alc-device]
    (when-not (zero? device)
      (ALC11/alcCloseDevice device)))
  (ALC/destroy))

(defn suspend-context
  "Pauses updates to the state of all objects until the context is resumed.
  May be used to suspend updates to sources during the execution of audio update
  code."
  []
  (when-let [ctx @al-context]
    (when-not (zero? ctx)
      (ALC11/alcSuspendContext ctx))))

(defn resume-context
  "Resumes updates to the state of objects.
  Called after [[suspend-context]] to apply any state changes."
  []
  (when-let [ctx @al-context]
    (when-not (zero? ctx)
      (ALC11/alcProcessContext ctx))))

(deftype Sound [id]
  Resource
  (free [_]
    (AL11/alDeleteBuffers id)))

(deftype AudioSource [id]
  Resource
  (free [_]
    (AL11/alDeleteSources id)))

(defn make-sound
  "Imports the given sound into a [[Sound]] resource."
  [file-or-buffer]
  (with-stack-allocator
    (let [channels (.asIntBuffer (alloc-bytes Integer/SIZE))
          sample-rate (.asIntBuffer (alloc-bytes Integer/SIZE))
          audio-buffer (STBVorbis/stb_vorbis_decode_filename file-or-buffer channels sample-rate)]
      (try
        (let [channels (.get channels)
              sample-rate (.get sample-rate)
              format (cond
                       (= channels 1) AL11/AL_FORMAT_MONO16
                       (= channels 2) AL11/AL_FORMAT_STEREO16
                       :otherwise -1)
              sound (AL11/alGenBuffers)]
          (AL11/alBufferData sound format audio-buffer sample-rate)
          (Sound. sound))
        (finally
          (LibCStdlib/free audio-buffer))))))

(defn make-audio-source
  "Creates an audio source for playback."
  []
  (let [source (AL11/alGenSources)]
    (AudioSource. source)))

(defn source-play
  "Marks an audio source as playing."
  [source]
  (AL11/alSourcePlay (.-id source)))

(defn source-assign-sound
  "Updates an audio source to play the passed sound.
  This replaces any previously-played sound on this source."
  [source sound]
  (AL11/alSourcei (.-id source) AL11/AL_BUFFER (.-id sound)))
