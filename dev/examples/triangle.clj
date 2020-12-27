(ns examples.triangle
  (:require
   [examples.window :as e.w]
   [s-expresso.memory :as mem :refer [with-stack-allocator]]
   [s-expresso.mesh :as m]
   [s-expresso.resource :refer [with-free]]
   [s-expresso.shader :as sh]
   [s-expresso.window :as w]
   [taoensso.timbre :as log])
  (:import
   (org.lwjgl.opengl
    GL GL45
    GLDebugMessageCallback GLDebugMessageCallbackI)
   (org.lwjgl.system
    MemoryStack)))

(defn step
  [window mesh]
  (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))

  ;; do rendering
  (GL45/glDrawArrays (:element-type mesh)
                     (:start-offset mesh)
                     (:element-count mesh))

  (w/swap-buffers window)
  (w/poll-events))

(def vert-shader
  {:source "
#version 450 core
layout (location=0) in vec3 aPos;
layout (location=1) in vec3 aCol;

out vec3 vCol;

void main()
{
    vCol = aCol;
    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
}
"
   :stage :vertex})

(def frag-shader
  {:source "
#version 450 core

in vec3 vCol;

out vec4 fragColor;
void main()
{
    fragColor = vec4(vCol.x, vCol.y, vCol.z, 1.0f);
}
"
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
    (sh/bind-shader-program shader-program)
    (GL45/glBindVertexArray (:vao-id mesh))
    (while (not (w/window-should-close? window))
      (step window mesh))
    (sh/bind-shader-program nil))
  window)

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (window-loop)
      (e.w/shutdown-window))
  (e.w/shutdown))
