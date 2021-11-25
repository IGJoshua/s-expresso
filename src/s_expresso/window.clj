(ns s-expresso.window
  "Functions for creating and managing a window with an OpenGL context.

  The system needs to be initialized before any of the functions are called,
  with [[init-glfw]], and once everything is shut down [[shutdown-glfw]] should
  be called to ensure all resources are returned to the windowing system.

  All functions in this namespace should be called from the main thread, except
  for the time functions. This will prevent any issues with cross-platform
  windowing.

  Before any OpenGL rendering is done on a thread, a context must be made
  current for that thread. Only one context can be created per window, however
  if a window is created with a parent and is never shown, it can be used as an
  additional context, and passed to another thread for its use. However, only
  one context may be current at a time, and each thread should release its
  context before another thread takes one."
  (:refer-clojure :exclude [time])
  (:require
   [clojure.reflect :as refl]
   [clojure.string :as str]
   [farolero.core :as far]
   [s-expresso.resource :refer [Resource]])
  (:import
   (org.lwjgl.glfw
    Callbacks GLFW
    GLFWCharCallbackI GLFWCursorEnterCallbackI
    GLFWCursorPosCallbackI GLFWDropCallback
    GLFWDropCallbackI GLFWErrorCallback
    GLFWErrorCallbackI GLFWFramebufferSizeCallbackI
    GLFWJoystickCallbackI GLFWKeyCallbackI
    GLFWMouseButtonCallbackI GLFWScrollCallbackI
    GLFWWindowCloseCallbackI GLFWWindowFocusCallbackI
    GLFWWindowSizeCallbackI)))

(defn init-glfw
  "Initializes GLFW for use on the system.

  Returns a boolean representing if GLFW was successfully initialized.

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
     (GLFW/glfwInit))))

(defn shutdown-glfw
  "Terminates GLFW and frees any resources specific to it."
  []
  (when-let [callback (GLFW/glfwSetErrorCallback nil)]
    (.free callback))
  (GLFW/glfwTerminate))

(deftype Window [id]
  Resource
  (free [_]
    (Callbacks/glfwFreeCallbacks id)
    (GLFW/glfwDestroyWindow id)
    nil))

(def ^:private mod-bit->mod
  "Map from the modifier value in GLFW to a keyword modifier name."
  {GLFW/GLFW_MOD_ALT :alt
   GLFW/GLFW_MOD_CONTROL :control
   GLFW/GLFW_MOD_SHIFT :shift
   GLFW/GLFW_MOD_SUPER :super
   GLFW/GLFW_MOD_CAPS_LOCK :caps-lock
   GLFW/GLFW_MOD_NUM_LOCK :num-lock})

(def mods
  "Set of all possible mods."
  (set (vals mod-bit->mod)))

(defn- mod-bits-set
  "Takes an integer representing a bitset and returns a set of modifiers"
  [mod-int]
  (set (map mod-bit->mod (filter #(not (zero? (bit-and % mod-int))) (keys mod-bit->mod)))))

(def ^:private action-int->action
  "Map from the action value in GLFW to a keyword action name."
  {GLFW/GLFW_PRESS :press
   GLFW/GLFW_RELEASE :release
   GLFW/GLFW_REPEAT :repeat})

(def actions
  "Set of all possible actions"
  (set (vals action-int->action)))

(def ^:private key-int->key
  "Map from the key value in GLFW to a keyword key name."
  (let [key-member-names (filter #(str/starts-with? % "GLFW_KEY_")
                                 (map (comp name :name)
                                      (:members (refl/type-reflect GLFW))))]
    (into {}
          (for [key-member-name key-member-names]
            [(.get (.getField GLFW key-member-name) nil)
             ((comp keyword
                    #(str/replace % "_" "-")
                    str/lower-case
                    #(subs % 9))
              key-member-name)]))))

(def key-names
  "Set of all named keys."
  (set (vals key-int->key)))

(def ^:private cursor-mode->mode-int
  "Map from a cursor mode keyword to the cursor mode for GLFW."
  {:normal GLFW/GLFW_CURSOR_NORMAL
   :hidden GLFW/GLFW_CURSOR_HIDDEN
   :disabled GLFW/GLFW_CURSOR_DISABLED})

(def cursor-modes
  "Set of all possible cursor modes."
  (set (keys cursor-mode->mode-int)))

(def ^:private mouse-button-int->mouse-button
  "Takes a mouse button int from GLFW and returns a keyword to represent it."
  {GLFW/GLFW_MOUSE_BUTTON_1 :mb1
   GLFW/GLFW_MOUSE_BUTTON_2 :mb2
   GLFW/GLFW_MOUSE_BUTTON_3 :mb3
   GLFW/GLFW_MOUSE_BUTTON_4 :mb4
   GLFW/GLFW_MOUSE_BUTTON_5 :mb5
   GLFW/GLFW_MOUSE_BUTTON_6 :mb6
   GLFW/GLFW_MOUSE_BUTTON_7 :mb7
   GLFW/GLFW_MOUSE_BUTTON_8 :mb8})

(def mouse-buttons
  "Vector of all named mouse buttons."
  (vec (sort (vals mouse-button-int->mouse-button))))

(defn set-window-cursor-mode
  "Sets a given cursor mode on the given window.

  Valid values for `cursor-mode` are `:normal`, `:hidden`, and `:disabled`."
  [window cursor-mode]
  (GLFW/glfwSetInputMode (.-id window) GLFW/GLFW_CURSOR (cursor-mode->mode-int cursor-mode)))

(defn set-raw-mouse-motion
  "Sets the mouse to use raw input for a given window."
  [window raw-mouse-motion]
  (if raw-mouse-motion
    (when (GLFW/glfwRawMouseMotionSupported)
      (GLFW/glfwSetInputMode (.-id window) GLFW/GLFW_RAW_MOUSE_MOTION GLFW/GLFW_TRUE))
    (GLFW/glfwSetInputMode (.-id window) GLFW/GLFW_RAW_MOUSE_MOTION GLFW/GLFW_FALSE)))

(defn make-window
  "Creates a window with the given options, returning a `Window` record.

  Returns nil if the window could not be created.

  The options map takes the following keys:
  | key                       | description |
  |---------------------------|-------------|
  | `:key-callback`           | Callback function which takes the window, key code, scan code, action, and modifier keys and performs any side effects from the keypress; default: sets window to close on escape being released
  | `:resize-callback`        | Callback function which takes the window, width, and height, and performs side effects from the resize event; default: nil
  | `:framebuffer-callback`   | Callback function which takes the window, width, and height, and performs side effects from resizing the framebuffer; default: nil
  | `:char-callback`          | Callback function which takes the window and a string representing the input text, and performs side effects from the text input; default: nil
  | `:cursor-pos-callback`    | Callback function which takes the window, x, and y position of the mouse, and performs side effects from the mouse move event; default: nil
  | `:cursor-enter-callback`  | Callback function which takes the window and a boolean of if the mouse is in the window; default: nil
  | `:mouse-button-callback`  | Callback function which takes the window, button code, action, and modifier keys, and performs any side effects from the mouse press; default: nil
  | `:scroll-callback`        | Callback function which takes the window, an x offset, and a y offset, and performs any side effects from the scroll event; default: nil
  | `:file-callback`          | Callback function which takes the window and a vector of file paths, and performs any side effects from the user dropping a file into the window; default: nil
  | `:request-close-callback` | Callback function which takes the window, and performs any side effects from the user requesting the application close; default: nil
  | `:focus-callback`         | Callback function which takes the window and a boolean of if the window is now focused, and performs side effects from the focus event; default: nil
  | `:cursor-mode`            | Cursor mode for the window, can be `:normal`, `:hidden`, or `:disabled`; default: `:normal`
  | `:raw-mouse-motion`       | If the cursor mode is `:disabled` and this is true, then mouse motion will be given without processing for cursor motion; default: false
  | `:size`                   | Dimensions of the window; default: [800 600]
  | `:monitor`                | Monitor id to create a fullscreen window on, 0 or nil when windowed; default: nil
  | `:title`                  | Title text of the window; default: \"Clojure GLFW Window\"
  | `:icon`                   |
  | `:parent-window`          | Window record of the parent window to share OpenGL context and resources with; default: nil
  | `:gl-major-version`       | The major version of OpenGL to request the context be made with; default: 4
  | `:gl-minor-version`       | The minor version of OpenGL to request the context be made with; default: 5
  | `:resizable`              | Whether or not the window is resizable; default: true
  | `:debug-context`          | If true, a debug context with additional reporting will be created; default: false
  | `:no-error`               | If true, GL errors will not be generated by the context, instead producing undefined behavior, and potentially improving performance; default: false
  | `:min-size`               | Minimum dimensions of the window; default: [400 300]
  | `:max-size`               | Maximum dimensinos of the window; default: [1280 720]"
  ([] (make-window {}))
  ([opts]
   (let [{:keys [key-callback resize-callback
                 framebuffer-callback
                 char-callback cursor-pos-callback
                 cursor-enter-callback mouse-button-callback
                 scroll-callback file-callback
                 request-close-callback focus-callback
                 cursor-mode raw-mouse-motion
                 size monitor
                 title parent-window
                 gl-major-version gl-minor-version
                 resizable debug-context no-error
                 min-size max-size]
          :or {size [800 600]
               title "Clojure GLFW Window"
               key-callback (fn [window key _ action _]
                              (when (and (= key :escape)
                                         (= action :release))
                                (GLFW/glfwSetWindowShouldClose (.-id window) true)))
               cursor-mode :normal
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
     (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_NO_ERROR (if no-error GLFW/GLFW_TRUE GLFW/GLFW_FALSE))
     (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR gl-major-version)
     (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR gl-minor-version)

     ;; create the window
     (far/block return
       (let [vid-mode (when monitor
                        (GLFW/glfwGetVideoMode monitor))
             size (if monitor
                    [(.width vid-mode) (.height vid-mode)]
                    size)
             _ (when monitor
                 (GLFW/glfwWindowHint GLFW/GLFW_RED_BITS (.redBits vid-mode))
                 (GLFW/glfwWindowHint GLFW/GLFW_GREEN_BITS (.greenBits vid-mode))
                 (GLFW/glfwWindowHint GLFW/GLFW_BLUE_BITS (.blueBits vid-mode))
                 (GLFW/glfwWindowHint GLFW/GLFW_REFRESH_RATE (.refreshRate vid-mode))
                 (GLFW/glfwWindowHint GLFW/GLFW_AUTO_ICONIFY GLFW/GLFW_FALSE))
             id (GLFW/glfwCreateWindow (int (first size)) (int (second size))
                                       ^CharSequence title
                                       (long (or monitor 0))
                                       (long (or (when parent-window (.-id parent-window)) 0)))
             window (->Window id)]
         (when (or (nil? id)
                   (= id 0))
           (far/return-from return))

         (GLFW/glfwSetInputMode id GLFW/GLFW_LOCK_KEY_MODS GLFW/GLFW_TRUE)
         (set-window-cursor-mode window cursor-mode)
         (set-raw-mouse-motion window (and (= :disabled cursor-mode) raw-mouse-motion))

         ;; set the callbacks
         (when key-callback
           (GLFW/glfwSetKeyCallback
            id
            (reify GLFWKeyCallbackI
              (invoke [this wnd key scancode action mods]
                (key-callback window (key-int->key key)
                              scancode (action-int->action action)
                              (mod-bits-set mods))))))

         (when resize-callback
           (GLFW/glfwSetWindowSizeCallback
            id
            (reify GLFWWindowSizeCallbackI
              (invoke [this wnd width height]
                (resize-callback window width height)))))

         (when framebuffer-callback
           (GLFW/glfwSetFramebufferSizeCallback
            id
            (reify GLFWFramebufferSizeCallbackI
              (invoke [this wnd width height]
                (framebuffer-callback window width height)))))

         (when char-callback
           (GLFW/glfwSetCharCallback
            id
            (reify GLFWCharCallbackI
              (invoke [this wnd codepoint]
                (char-callback window (String. (Character/toChars codepoint)))))))

         (when cursor-pos-callback
           (GLFW/glfwSetCursorPosCallback
            id
            (reify GLFWCursorPosCallbackI
              (invoke [this wnd xpos ypos]
                (cursor-pos-callback window xpos ypos)))))

         (when cursor-enter-callback
           (GLFW/glfwSetCursorEnterCallback
            id
            (reify GLFWCursorEnterCallbackI
              (invoke [this wnd entered]
                (cursor-enter-callback window entered)))))

         (when mouse-button-callback
           (GLFW/glfwSetMouseButtonCallback
            id
            (reify GLFWMouseButtonCallbackI
              (invoke [this wnd button action mods]
                (mouse-button-callback window
                                       (mouse-button-int->mouse-button button)
                                       (action-int->action action)
                                       (mod-bits-set mods))))))

         (when scroll-callback
           (GLFW/glfwSetScrollCallback
            id
            (reify GLFWScrollCallbackI
              (invoke [this wnd xoffset yoffset]
                (scroll-callback window
                                 xoffset yoffset)))))

         (when file-callback
           (GLFW/glfwSetDropCallback
            id
            (reify GLFWDropCallbackI
              (invoke [this wnd cnt paths]
                (file-callback window
                               (mapv #(GLFWDropCallback/getName paths %) (range cnt)))))))

         (when request-close-callback
           (GLFW/glfwSetWindowCloseCallback
            id
            (reify GLFWWindowCloseCallbackI
              (invoke [this wnd]
                (request-close-callback window)))))

         (when focus-callback
           (GLFW/glfwSetWindowFocusCallback
            id
            (reify GLFWWindowFocusCallbackI
              (invoke [this wnd focused?]
                (focus-callback window
                                focused?)))))

         (GLFW/glfwSetWindowSizeLimits
          id
          (if (#{:dont-care} (first min-size))
            GLFW/GLFW_DONT_CARE
            (first min-size))
          (if (#{:dont-care} (second min-size))
            GLFW/GLFW_DONT_CARE
            (second min-size))
          (if (#{:dont-care} (first max-size))
            GLFW/GLFW_DONT_CARE
            (first max-size))
          (if (#{:dont-care} (second max-size))
            GLFW/GLFW_DONT_CARE
            (second max-size)))

         window)))))

(defn center-window
  "Centers a window on the given monitor (default primary)."
  ([window] (center-window window nil))
  ([window monitor]
   (let [width (int-array 1)
         height (int-array 1)
         _ (GLFW/glfwGetWindowSize ^long (.-id window) ^ints width ^ints height)
         vid-mode (GLFW/glfwGetVideoMode (or monitor (GLFW/glfwGetPrimaryMonitor)))]
     (GLFW/glfwSetWindowPos
      (.-id window)
      (/ (- (.width vid-mode) (first width)) 2)
      (/ (- (.height vid-mode) (first height)) 2)))
   window))

(defn show-window
  "Takes a window and shows it.

  Returns the window."
  [window]
  (GLFW/glfwShowWindow (.-id window))
  window)

(defn hide-window
  "Takes a window and hides it.

  Returns the window."
  [window]
  (GLFW/glfwHideWindow (.-id window))
  window)

(defn visible?
  "Checks to see if the `window` is visible."
  [window]
  (= (GLFW/glfwGetWindowAttrib (.-id window) GLFW/GLFW_VISIBLE) GLFW/GLFW_TRUE))

(defn focus-window
  "Forces the `window` to take input focus.

  Returns the window."
  [window]
  (GLFW/glfwFocusWindow (.-id window))
  window)

(defn focused?
  "Checks to see if the `window` has input focus."
  [window]
  (= (GLFW/glfwGetWindowAttrib (.-id window) GLFW/GLFW_FOCUSED) GLFW/GLFW_TRUE))

(defn request-attention
  "Highlights the `window` or application to bring it to the user's attention.

  Returns the window."
  [window]
  (GLFW/glfwRequestWindowAttention (.-id window))
  window)

(defn make-context-current-to-window
  "Takes a window and makes the OpenGL context current to that window.

  If window is nil, this releases the context which is current on the thread."
  [window]
  (GLFW/glfwMakeContextCurrent (when window (.-id window)))
  window)

(defn set-vsync
  "Sets vsync to swap at every `interval` vblanks.

  If `interval` is a non-numeric truthy value, defaults to 1, and 0 if falsey."
  [interval]
  (GLFW/glfwSwapInterval (or (if-not (number? interval)
                               (and interval 1)
                               (int interval))
                             0)))

(defn poll-events
  "Polls for GLFW events.

  This should be called periodically to ensure that input callbacks on windows
  are called, and to ensure the desktop environment knows the program is
  responsive. Should never block, but all callbacks are called on this thread
  before returning."
  []
  (GLFW/glfwPollEvents))

(defn swap-buffers
  "Swaps the framebuffer on the given window.

  This call will block until a future v-blank based on your vsync
  configuration. Returns the window."
  [window]
  (GLFW/glfwSwapBuffers (.-id window))
  window)

(defn window-should-close?
  "Predicate for if a window should close."
  [window]
  (GLFW/glfwWindowShouldClose (.-id window)))

(defn window-should-close
  "Sets if a window should close.

  Returns the window."
  [window should-close?]
  (GLFW/glfwSetWindowShouldClose (.-id window) should-close?)
  window)

(defn time
  "Gets the time in seconds.

  Counts from the passed value, or starts with 0 at when glfw-init was called."
  ([start] (GLFW/glfwSetTime (double start)))
  ([] (GLFW/glfwGetTime)))

(defn raw-time
  "Gets the time in raw temporal units.

  The current time in seconds will be this value divided by the value
  of [[raw-time-frequency]]"
  []
  (GLFW/glfwGetTimerValue))

(defn raw-time-frequency
  "Gets the frequency of the raw timer."
  []
  (GLFW/glfwGetTimerFrequency))

(defn windowed?
  "Checks if the window is in windowed mode."
  [window]
  (zero? (GLFW/glfwGetWindowMonitor (.-id window))))

(defn fullscreen?
  "Checks if the window is in fullscreen mode."
  [window]
  (not (windowed? window)))

(defn fullscreen-monitor
  "Gets the monitor the fullscreen window is currently on.

  Returns an opaque handle to the monitor, or nil if the window is not
  fullscreen."
  [window]
  (let [monitor (GLFW/glfwGetWindowMonitor (.-id window))]
    (when-not (zero? monitor)
      monitor)))

(defn video-mode
  "Gets the current video mode for the `monitor`.

  This is a map with the keys `:width`, `:height`, `:red-bits`, `:green-bits`,
  `:blue-bits`, and `:refresh-rate`."
  [monitor]
  (let [vid-mode (GLFW/glfwGetVideoMode monitor)]
    {:width (.width vid-mode)
     :height (.height vid-mode)
     :red-bits (.redBits vid-mode)
     :green-bits (.greenBits vid-mode)
     :blue-bits (.blueBits vid-mode)
     :refresh-rate (.refreshRate vid-mode)}))

(def ^:private event-id->connection-event
  "Map from GLFW joystick connection event ids to keywords representing them."
  {GLFW/GLFW_CONNECTED :connected
   GLFW/GLFW_DISCONNECTED :disconnected})

(defn set-joystick-callback
  "Sets the global joystick connection event callback.

  While these events do not require a window, [[poll-events]] must be called for
  them to be consistently processed. If `callback` is nil, then the callback is
  unset."
  [callback]
  (GLFW/glfwSetJoystickCallback
   (when callback
     (reify GLFWJoystickCallbackI
       (invoke [this joystick-id event]
         (callback joystick-id (event-id->connection-event event)))))))
