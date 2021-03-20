(ns examples.textured-quad
  (:require
   [cljsl.compiler :as c]
   [examples.window :as e.w]
   [examples.triangle :as e.t]
   [s-expresso.memory :refer [with-stack-allocator]]
   [s-expresso.mesh :as m]
   [s-expresso.resource :refer [with-free]]
   [s-expresso.shader :as sh]
   [s-expresso.texture :as tex]
   [s-expresso.window :as w])
  (:import
   (org.lwjgl.opengl
    GL GL45)))

(defn step
  [window mesh]
  (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))

  (m/draw-mesh mesh)

  (w/swap-buffers window)
  (w/poll-events))

(c/defparam v-pos "vec3"
  :layout {"location" 0})
(c/defparam v-uv "vec2"
  :layout {"location" 1})
(c/defparam uv "vec2")

(c/defshader vert-source
  {v-pos :in
   v-uv :in
   uv :out}
  (set! gl_Position (vec4 (:xyz v-pos) 1))
  (set! uv v-uv))

(def vert-shader
  {:source (::c/source vert-source)
   :stage :vertex})

(c/defparam frag-color "vec4")

(c/defuniform sam "sampler2D")

(c/defshader frag-source
  {uv :in
   frag-color :out}
  (set! frag-color (texture sam uv)))

(def frag-shader
  {:source (::c/source frag-source)
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
              image (tex/load-image "res/textures/octostone/octostoneAlbedo.png" 3)
              texture (tex/make-texture {:format GL45/GL_RGB8
                                         :dimensions (:dimensions image)}
                                        {:data (:data image)
                                         :format GL45/GL_RGB
                                         :type GL45/GL_UNSIGNED_BYTE})]
    (sh/with-shader-program shader-program
      (tex/with-texture texture 0
        (sh/upload-uniform-int shader-program (c/sym->ident `sam) 0)
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
