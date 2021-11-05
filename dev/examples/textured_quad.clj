(ns examples.textured-quad
  (:require
   [cljsl.compiler :as c]
   [examples.window :as e.w]
   [examples.triangle :as e.t]
   [s-expresso.ecs :as ecs]
   [s-expresso.mesh :as m]
   [s-expresso.render :as r]
   [s-expresso.shader :as sh]
   [s-expresso.texture :as tex]))

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

(defn texture-resolver
  [path components image-format texture-format]
  #(future
     (let [image (tex/load-image path components)]
       (delay
         (tex/make-texture {:internal-format image-format
                            :dimensions (:dimensions image)}
                           (assoc texture-format
                                  :data (:data image)))))))

(def ^:private shader-program (e.t/shader-program-resolver [vert-shader frag-shader]))
(def ^:private quad-mesh (e.t/mesh-resolver quad-mesh-data pos-mesh-layout))
(def ^:private tex (texture-resolver "res/textures/octostone/octostoneAlbedo.png"
                                     3
                                     :rgb8
                                     {:format :rgb
                                      :data-type :unsigned-byte}))

(defn render-entity
  [_entity]
  (reify r/RenderOp
    (op-deps [_]
      {::quad quad-mesh
       ::shader-program shader-program
       ::texture tex})
    (apply-op! [_ {{::keys [quad shader-program texture]} ::r/resources}]
      (when (and quad shader-program texture)
        (sh/with-shader-program shader-program
          (tex/with-texture texture 0
            (sh/upload-uniform-int shader-program (c/sym->ident `sam) 0)
            (m/draw-mesh quad)))))))

(defn- draw-mesh
  [game-state]
  (->> (::ecs/entities game-state)
       vals
       (filter ::e.t/position)
       (map render-entity)))

(def init-game-state (assoc e.t/init-game-state
                            ::r/systems [#'e.t/clear-screen #'draw-mesh]
                            ::ecs/systems [#'e.t/ingest-input]))

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (e.t/run-sim init-game-state e.t/init-render-state)
      (e.w/shutdown-window))
  (e.w/shutdown))
