(ns examples.triangle
  (:require
   [cljsl.compiler :as c]
   [examples.window :as e.w]
   [s-expresso.memory :refer [with-stack-allocator]]
   [s-expresso.mesh :as m]
   [s-expresso.resource :refer [with-free]]
   [s-expresso.shader :as sh]
   [s-expresso.window :as w]
   [taoensso.timbre :as log])
  (:import
   (org.lwjgl.opengl
    GL GL45
    GLDebugMessageCallback GLDebugMessageCallbackI)))

(defn step
  [window mesh]
  (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))

  (m/draw-mesh mesh)

  (w/swap-buffers window)
  (w/poll-events))

(c/defparam a-pos "vec3"
  :layout {"location" 0})
(c/defparam a-col "vec3"
  :layout {"location" 1})
(c/defparam v-col "vec3")

(c/defshader vert-source
  {a-pos :in
   a-col :in
   v-col :out}
  (set! v-col a-col)
  (set! gl_Position (vec4 (:xyz a-pos) 1.0)))

(def vert-shader
  {:source (::c/source vert-source)
   :stage :vertex})

(c/defparam frag-color "vec4")

(c/defshader frag-source
  {v-col :in
   frag-color :out}
  (set! frag-color (vec4 (:xyz v-col) (float 1.0))))

(def frag-shader
  {:source (::c/source frag-source)
   :stage :fragment})

(def quad-mesh-data {:vertices [{:pos [-0.5 -0.5 0.0]
                                 :col [1.0 0.0 0.0]}
                                {:pos [0.5 -0.5 0.0]
                                 :col [0.0 1.0 0.0]}
                                {:pos [0.0 0.5 0.0]
                                 :col [0.0 0.0 1.0]}]})

(def pos-mesh-layout {:buffer-layouts [{:attrib-layouts [{:name :pos
                                                          :type :float
                                                          :count 3}
                                                         {:name :col
                                                          :type :float
                                                          :count 3}]}]
                      :element-type :triangles})

(defn enable-debug-logging
  [window]
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

(defn window-loop
  [window]
  ;; init anything on the opengl side
  (enable-debug-logging window)

  (GL45/glClearColor 0 0 0 1)
  (GL45/glClearDepth 1)
  (with-free [mesh (with-stack-allocator
                     (m/make-mesh pos-mesh-layout (m/pack-verts pos-mesh-layout quad-mesh-data)))
              shader-program (sh/make-shader-program-from-sources [vert-shader frag-shader])]
    (sh/with-shader-program shader-program
      (while (not (w/window-should-close? window))
        (step window mesh))))
  window)

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (window-loop)
      (e.w/shutdown-window))
  (e.w/shutdown))
