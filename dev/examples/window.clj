(ns examples.window
  (:require
   [s-expresso.resource :as res]
   [s-expresso.window :as wnd]
   [taoensso.timbre :as log])
  (:import
   (org.lwjgl.opengl
    GL GL45
    GLDebugMessageCallback GLDebugMessageCallbackI)))

(defn init
  []
  (wnd/init-glfw))

(defn shutdown
  []
  (wnd/shutdown-glfw))

(def window (atom nil))
(def window-opts
  {:key-callback (fn [_window key _scancode action mods]
                   (prn key)
                   (prn action)
                   (prn mods))
   :cursor-pos-callback (fn [_window xpos ypos]
                          (prn xpos ypos))
   :mouse-button-callback (fn [_window button action mods]
                            (prn button)
                            (prn action)
                            (prn mods))
   :cursor-mode :hidden
   :debug-context true
   :title "Window Test"})

(defn- enable-debug-logging!
  []
  (let [flags (int-array 1)]
    (GL45/glGetIntegerv GL45/GL_CONTEXT_FLAGS flags)
    (when-not (zero? (bit-and GL45/GL_CONTEXT_FLAG_DEBUG_BIT
                              (first flags)))
      (GL45/glEnable GL45/GL_DEBUG_OUTPUT)
      (GL45/glEnable GL45/GL_DEBUG_OUTPUT_SYNCHRONOUS)
      (GL45/glDebugMessageControl GL45/GL_DONT_CARE
                                  GL45/GL_DONT_CARE
                                  GL45/GL_DONT_CARE
                                  (int-array 0)
                                  true)
      (GL45/glDebugMessageCallback
       (reify GLDebugMessageCallbackI
         (invoke [this source type id severity length message user-param]
           (log/debug (GLDebugMessageCallback/getMessage length message))))
       0))))

(defn start-window
  [opts]
  (let [wnd (-> (wnd/make-window opts)
                (wnd/make-context-current-to-window)
                (wnd/center-window)
                (wnd/show-window))]
    (reset! window wnd)
    (wnd/set-vsync true)
    (GL/createCapabilities)
    (enable-debug-logging!)
    wnd))

(defn step-window
  [window]
  (GL45/glClearColor 0 0 0 1)
  (GL45/glClearDepth 1)
  (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))
  (wnd/swap-buffers window)
  (wnd/poll-events))

(defn shutdown-window
  [wnd]
  (res/free wnd)
  (reset! window nil))

(defn- window-loop
  [window]
  (while (not (wnd/window-should-close? window))
    (step-window window))
  window)

(defn start
  []
  (init)
  (-> (start-window window-opts)
      (window-loop)
      (shutdown-window))
  (shutdown))
