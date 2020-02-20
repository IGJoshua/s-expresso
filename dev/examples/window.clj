(ns examples.window
  (:require
   [s-expresso.resource :as res]
   [s-expresso.window :as wnd])
  (:import
   (org.lwjgl.opengl
    GL GL11)))

(defn init
  []
  (wnd/init-glfw))

(defn shutdown
  []
  (wnd/shutdown-glfw))

(def window (atom nil))
(def window-opts
  {:key-callback (fn [window key scancode action mods]
                   (prn key)
                   (prn action)
                   (prn mods))
   :cursor-pos-callback (fn [window xpos ypos]
                          (prn xpos ypos))
   :mouse-button-callback (fn [window button action mods]
                            (prn button)
                            (prn action)
                            (prn mods))
   :cursor-mode :hidden
   :debug-context true
   :title "Window Test"})

(defn start-window
  [opts]
  (let [wnd (-> (wnd/make-window opts)
                (wnd/make-context-current-to-window)
                (wnd/center-window)
                (wnd/show-window))]
    (reset! window wnd)
    (wnd/set-vsync true)
    (GL/createCapabilities)
    wnd))

(defn step-window
  [window]
  (GL11/glClearColor 0 0 0 1)
  (GL11/glClearDepth 1)
  (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
  (wnd/swap-buffers window)
  (wnd/poll-events))

(defn shutdown-window
  [wnd]
  (res/free wnd)
  (reset! window nil))

(defn window-loop
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
