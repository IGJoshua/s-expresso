(ns s-expresso.window
  "Functions for creating and managing a window with an OpenGL context."
  (:require
   [s-expresso.resource :refer [Resource]])
  (:import
   (org.lwjgl.glfw
    Callbacks GLFW
    GLFWErrorCallback)))

(defn init-glfw
  "Initializes GLFW for use on the system.

  The options map takes the following keys:
  | key               | description |
  |-------------------|-------------|
  | `:error-callback` | Callback function which takes the error code and a description of the error and handles the error, or a `java.io.OutputStream` to output the error message to; default: java.lang.System.err"
  ([] (init-glfw {}))
  ([opts]
   (let [{:keys [error-callback]
          :or {error-callback System/err}}
         opts]
     (when error-callback
       (.set (if (instance? java.io.OutputStream error-callback)
               (GLFWErrorCallback/createPrint error-callback)
               (GLFWErrorCallback/create (reify GLFWErrorCallbackI
                                           (invoke [this error description]
                                             (error-callback error description)))))))
     (when-not (GLFW/glfwInit)
       (throw (ex-info "Unable to initialize GLFW" {}))))))

(defn shutdown-glfw
  "Terminates GLFW and frees any resources specific to it."
  []
  (when-let [callback (GLFW/glfwSetErrorCallback nil)]
    (.free callback))
  (GLFW/glfwTerminate))

(defrecord Window [id]
  Resource
  (free [_]
    (Callbacks/glfwFreeCallbacks id)
    (GLFW/glfwDestroyWindow id)
    nil))

(defn make-window
  "Creates a window with the given options, returning a `Window` record.

  The options map takes the following keys:
  | key                      | description |
  |--------------------------|-------------|
  | `:cursor-mode`           | Cursor mode for the window, can be `:normal`, `:hidden`, or `:disabled`; default: `:normal`
  | `:raw-mouse-motion`      | If the cursor mode is `:disabled` and this is true, then mouse motion will be given without processing for cursor motion; default: false
  | `:size`                  | Dimensions of the window; default: [800 600]
  | `:monitor`               | Monitor id to create a fullscreen window on, 0 or nil when windowed; default: nil
  | `:title`                 | Title text of the window; default: \"Clojure GLFW Window\"
  | `:parent-window`         | Window record of the parent window to share OpenGL context and resources with; default: nil
  | `:gl-major-version`      | The major version of OpenGL to request the context be made with; default: 4
  | `:gl-minor-version`      | The minor version of OpenGL to request the context be made with; default: 5
  | `:resizable`             | Whether or not the window is resizable; default: true
  | `:debug-context`         | If true, a debug context with additional reporting will be created; default: false
  | `:min-size`              | Minimum dimensions of the window; default: [400 300]
  | `:max-size`              | Maximum dimensinos of the window; default: [1280 720]"
  ([] (make-window {}))
  ([opts]
   (let [{:keys [cursor-mode raw-mouse-motion
                 size monitor
                 title parent-window
                 gl-major-version gl-minor-version
                 resizable debug-context
                 min-size max-size]
          :or {size [800 600]
               title "Clojure GLFW Window"
               gl-major-version 4
               gl-minor-version 5
               resizable true
               min-size [400 300]
               max-size [1280 720]}}
         opts]
     ;; set the default window settings
     (GLFW/glfwDefaultWindowHints)
     (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
     (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GLFW/GLFW_TRUE)

     (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE (if resizable GLFW/GLFW_TRUE GLFW/GLFW_FALSE))
     (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_DEBUG_CONTEXT (if debug-context GLFW/GLFW_TRUE GLFW/GLFW_FALSE))
     (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR gl-major-version)
     (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR gl-minor-version)

     ;; create the window
     (let [vid-mode (when monitor
                      (GLFW/glfwGetVideoMode monitor))
           size (if monitor
                  [(.width vid-mode) (.height vid-mode)]
                  size)
           _ (when monitor
               (GLFW/glfwWindowHint GLFW/GLFW_RED_BITS (.redBits vid-mode))
               (GLFW/glfwWindowHint GLFW/GLFW_GREEN_BITS (.greenBits vid-mode))
               (GLFW/glfwWindowHint GLFW/GLFW_BLUE_BITS (.blueBits vid-mode))
               (GLFW/glfwWindowHint GLFW/GLFW_REFRESH_RATE (.refreshRate vid-mode)))
           id (GLFW/glfwCreateWindow (first size) (second size)
                                     title (or monitor 0) (or (:id parent-window) 0))]
       (when (or (nil? id)
                 (= id 0))
         (throw (ex-info "Failed to create a window!" {})))

       (GLFW/glfwSetInputMode id GLFW/GLFW_LOCK_KEY_MODS GLFW/GLFW_TRUE)
       (GLFW/glfwSetInputMode id GLFW/GLFW_CURSOR (cursor-mode->mode-int cursor-mode))
       (when (and (= :disabled cursor-mode)
                  raw-mouse-motion
                  (GLFW/glfwRawMouseMotionSupported))
         (GLFW/glfwSetInputMode id GLFW/GLFW_RAW_MOUSE_MOTION GLFW/GLFW_TRUE))

       (GLFW/glfwSetWindowSizeLimits id
                                     (first min-size) (second min-size)
                                     (first max-size) (second max-size))

       ;; create the window object to return
       (->Window id)))))
