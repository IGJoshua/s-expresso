(ns s-expresso.window
  "Functions for creating and managing a window with an OpenGL context."
  (:import
   (org.lwjgl.glfw
    GLFW
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

