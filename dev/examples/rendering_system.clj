(ns examples.rendering-system
  (:require
   [examples.window :as e.w]
   [examples.triangle :as e.t]
   [s-expresso.memory :refer [with-stack-allocator]]
   [s-expresso.mesh :as m]
   [s-expresso.physics.dynamics :as d]
   [s-expresso.render :as r]
   [s-expresso.resource :refer [with-free]]
   [s-expresso.shader :as sh :refer [with-shader-program]]
   [s-expresso.window :as w]
   [s-expresso.ecs :as ecs :refer [defsystem]]
   [clojure.spec.alpha :as s])
  (:import
   (org.lwjgl.opengl
    GL GL45)))

(def vert-shader
  {:source "
#version 450 core

layout (location=0) in vec3 vPos;

uniform vec3 objCol;
uniform vec2 objPos;

out vec3 color;

void main()
{
    gl_Position = vec4(vec3(objPos, 0) + vPos, 1) * vec4(0.1, 0.1, 0, 1);
    color = objCol;
}
"
   :stage :vertex})

(def frag-shader
  {:source "
#version 450 core

in vec3 color;

out vec4 fragColor;

void main()
{
    fragColor = vec4(color, 1);
}
"
   :stage :fragment})

(def tri-mesh-data {:vertices [{:pos [-0.1 -0.1 0.0]}
                               {:pos [0.1 -0.1 0.0]}
                               {:pos [0.0 0.1 0.0]}]})
(def tri-mesh-layout {:buffer-layouts [{:attrib-layouts [{:name :pos
                                                          :type :float
                                                          :count 3}]}]
                      :element-type :triangles})

(defn render-entity
  [entity]
  (let [deps {:shader (reify r/Resolver
                        (step-resolver [t]
                          [t true])
                        (resolved-resource [_]
                          (sh/make-shader-program-from-sources [vert-shader frag-shader])))
              :mesh (reify r/Resolver
                      (step-resolver [t]
                        [t true])
                      (resolved-resource [_]
                        (with-stack-allocator
                          (m/make-mesh tri-mesh-layout (m/pack-verts tri-mesh-layout tri-mesh-data)))))}]
    (reify r/RenderOp
     (op-deps [_]
       deps)
     (apply-op! [_ st]
       (when-let [mesh (:mesh (::r/resources st))]
         (when-let [shader (:shader (::r/resources st))]
           (sh/with-shader-program shader
             (apply sh/upload-uniform-float shader "objPos" (::d/position entity))
             (apply sh/upload-uniform-float shader "objCol" (::color entity))
             (m/draw-mesh mesh))))))))

(defn state-to-ops
  [state]
  (cons
   (reify r/RenderOp
     (op-deps [_]
       {})
     (apply-op! [_ _]
       (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))))
   (for [[_ entity] (::ecs/entities state)]
     (render-entity entity))))

(def init-state {::ecs/entities {#uuid "a6239964-38d6-4f35-ac96-0cf831f49426"
                                 {::d/position [0 7]
                                  ::color [1 0 0]}
                                 #uuid "cb77ab65-5e4f-47c7-a08e-fa5bf39949a4"
                                 {::d/position [3 0]
                                  ::color [0 1 0]}
                                 #uuid "7917af55-5469-40e3-98f8-c6b494b3f6f2"
                                 {::d/position [0 -3]
                                  ::color [0 0 1]}}
                 ::ecs/systems []
                 ::r/systems [#'state-to-ops]})

(defn window-loop
  [window]
  ;; init anything on the opengl side
  (e.t/enable-debug-logging window)
  (w/set-vsync 1)

  (GL45/glClearColor 0 0 0 1)
  (GL45/glClearDepth 1)

  (loop [game-state init-state
         render-state {}]
    (let [game-state (ecs/step-scene game-state 0.016)
          render-state (r/step-renderer render-state game-state)]
      (when (not (w/window-should-close? window))
        (w/swap-buffers window)
        (w/poll-events)
        (recur game-state render-state))))

  window)

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (window-loop)
      (e.w/shutdown-window))
  (e.w/shutdown))
