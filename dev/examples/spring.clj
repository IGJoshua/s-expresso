(ns examples.spring
  (:require
   [clojure.core.matrix :as mat]
   [examples.window :as e.w]
   [examples.triangle :as e.t]
   [s-expresso.memory :as mem :refer [with-stack-allocator]]
   [s-expresso.mesh :as m]
   [s-expresso.resource :refer [with-free]]
   [s-expresso.shader :as sh :refer [with-shader-program]]
   [s-expresso.window :as w]
   [s-expresso.ecs :as ecs :refer [defsystem]]
   [s-expresso.physics.dynamics :as d]
   [s-expresso.physics.constraints :as constraint]
   [taoensso.timbre :as log])
  (:import
   (org.lwjgl.opengl
    GL GL45)))

(def gravity-vector (mat/array [0 -9.81]))

(defsystem gravity [::d/dimensions ::d/mass]
  [_ _ entity _]
  (d/add-force entity (mat/mul gravity-vector (::d/mass entity))))

(defsystem damping [::d/velocity]
  [_ _ entity _]
  (update entity ::d/velocity mat/mul 0.99)
  entity)

(defsystem spring-constraint [::constraints ::d/dimensions ::d/position]
  [scene _ entity _]
  (reduce
   (fn [body constraint]
     (d/add-force
      body
      (let [target (:target constraint)
            target-pos (if (uuid? target)
                         (::d/position ((::ecs/entities scene) target))
                         target)]
        (constraint/spring-force (::d/position body)
                                 target-pos
                                 (:distance constraint)
                                 (:constant constraint)))))
   entity
   (::constraints entity)))

(defsystem step-body [::d/dimensions ::d/position ::d/mass ::d/velocity]
  [_ _ entity dt]
  (d/step-body entity dt))

(def init-state {::ecs/entities {#uuid "a6239964-38d6-4f35-ac96-0cf831f49426"
                                 {::d/dimensions 2
                                  ::d/position [0 7]
                                  ::color [1 0 0]}
                                 #uuid "cb77ab65-5e4f-47c7-a08e-fa5bf39949a4"
                                 {::d/dimensions 2
                                  ::d/position [3 0]
                                  ::d/mass 5.0
                                  ::d/velocity [0 0]
                                  ::color [0 1 0]
                                  ::constraints [{:constant 30
                                                  :distance 3
                                                  :target #uuid "a6239964-38d6-4f35-ac96-0cf831f49426"}
                                                 {:constant 5
                                                  :distance 2
                                                  :target #uuid "7917af55-5469-40e3-98f8-c6b494b3f6f2"}]}
                                 #uuid "7917af55-5469-40e3-98f8-c6b494b3f6f2"
                                 {::d/dimensions 2
                                  ::d/position [0 -3]
                                  ::d/mass 3.0
                                  ::d/velocity [0 0]
                                  ::color [0 0 1]
                                  ::constraints [{:constant 15
                                                  :distance 2
                                                  :target #uuid "cb77ab65-5e4f-47c7-a08e-fa5bf39949a4"}]}}
                 ::ecs/systems [[#'gravity #'spring-constraint #'step-body #'damping]]})
(defonce state (atom init-state))

(comment

  ;; Set the state back to the init
  (do (reset! state init-state)
      nil)

  )

(defn step
  [window mesh shader-program]
  (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))

  (try
    ;; Step the ecs and draw
    (swap! state ecs/step-scene 0.016)
    (catch Exception e
      (log/error "Something went wrong in the step" e)))

  (doseq [[_ entity] (::ecs/entities @state)]
    (with-stack-allocator
      (let [color (mem/alloc-bytes (* Float/BYTES 3))
            position (mem/alloc-bytes (* Float/BYTES 2))]
        (mem/put-seq color (map float (::color entity)))
        (.flip color)
        (sh/upload-uniform-floats shader-program 3 "objCol" (.asFloatBuffer color))
        (mem/put-seq position (map float (::d/position entity)))
        (.flip position)
        (sh/upload-uniform-floats shader-program 2 "objPos" (.asFloatBuffer position))
        (m/draw-mesh mesh))))

  (w/swap-buffers window)
  (w/poll-events))

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

(defn window-loop
  [window]
  ;; init anything on the opengl side
  (e.t/enable-debug-logging window)

  (GL45/glClearColor 0 0 0 1)
  (GL45/glClearDepth 1)
  (with-free [mesh (with-stack-allocator
                     (m/make-mesh tri-mesh-layout (m/pack-verts tri-mesh-layout tri-mesh-data)))
              shader-program (sh/make-shader-program-from-sources [vert-shader frag-shader])]
    (with-shader-program shader-program
      (while (not (w/window-should-close? window))
        (step window mesh shader-program))))
  window)

(defn start
  []
  (e.w/init)
  (-> (e.w/start-window e.w/window-opts)
      (window-loop)
      (e.w/shutdown-window))
  (e.w/shutdown))
