(ns examples.textured-quad
  (:require
   [clojure.java.io :as io]
   [examples.window :as e.w]
   [examples.triangle :as e.t]
   [s-expresso.memory :as mem :refer [with-stack-allocator]]
   [s-expresso.mesh :as m]
   [s-expresso.resource :refer [with-free]]
   [s-expresso.shader :as sh]
   [s-expresso.texture :as tex]
   [s-expresso.window :as w]
   [taoensso.timbre :as log])
  (:import
   (java.io
    RandomAccessFile)
   (java.nio
    ByteBuffer)
   (java.nio.channels
    FileChannel FileChannel$MapMode)
   (org.lwjgl.opengl
    GL GL45
    GLDebugMessageCallback GLDebugMessageCallbackI)
   (org.lwjgl.stb
    STBImage)
   (org.lwjgl.system
    MemoryStack)))

(defn step
  [window mesh]
  (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))

  (m/draw-mesh mesh)

  (w/swap-buffers window)
  (w/poll-events))

(def vert-shader
  {:source "
#version 450 core

layout (location=0) in vec3 vPos;
layout (location=1) in vec2 vUV;

out vec2 uv;

void main()
{
    gl_Position = vec4(vPos.xyz, 1);
    uv = vUV;
}
"
   :stage :vertex})

(def frag-shader
  {:source "
#version 450 core

uniform sampler2D sam;
in vec2 uv;

out vec4 fragColor;

void main()
{
    fragColor = texture(sam, uv);
}
"
   :stage :fragment})

(def quad-mesh-data {:vertices [{:pos [-0.5 -0.5 0.0]
                                 :uv [0.0 0.0]}
                                {:pos [0.5 -0.5 0.0]
                                 :uv [1.0 0.0]}
                                {:pos [-0.5 0.5 0.0]
                                 :uv [0.0 1.0]}
                                {:pos [0.5 0.5 0.0]
                                 :uv [1.0 1.0]}]
                     :indices [0 1 2 2 1 3]})

(def pos-mesh-layout {:buffer-layouts [{:attrib-layouts [{:name :pos
                                                          :type :float
                                                          :count 3}
                                                         {:name :uv
                                                          :type :half-float
                                                          :count 2}]
                                        :interleaved true}]
                      :indices {}
                      :element-type :triangles})

(defn window-loop
  [window]
  (e.t/enable-debug-logging window)

  (GL45/glClearColor 0 0 0 1)
  (GL45/glClearDepth 1)
  (with-free [mesh (with-stack-allocator
                     (m/make-mesh pos-mesh-layout (m/pack-verts pos-mesh-layout quad-mesh-data)))
              shader-program (sh/make-shader-program-from-sources [vert-shader frag-shader])
              image (tex/load-image "textures/octostone/octostoneAlbedo.png" 3)
              texture (tex/make-texture {:format GL45/GL_RGB8
                                         :dimensions (:dimensions image)}
                                        {:data (:data image)
                                         :format GL45/GL_RGB
                                         :type GL45/GL_UNSIGNED_BYTE})]
    (sh/with-shader-program shader-program
      (tex/with-texture texture 0
        (sh/upload-uniform-int shader-program "sam" 0)
        (while (not (w/window-should-close? window))
          (step window mesh)))))
  window)

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (window-loop)
      (e.w/shutdown-window))
  (e.w/shutdown))
