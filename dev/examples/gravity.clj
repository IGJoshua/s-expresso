(ns examples.gravity
  (:require
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

(def gravity-constant 0.0015 #_(* 6.674 (Math/pow 10 -11)))

(defsystem gravity [::d/mass ::d/position]
  [scene entity-id entity dt]
  (let [other-bodies (filter (comp (partial some identity)
                                   (juxt ::d/position
                                         ::d/mass)
                                   second)
                             (::ecs/entities scene))
        forces (for [[id body] other-bodies
                     :when (not= id entity-id)]
                 (constraint/gravitational-force (::d/position entity)
                                                 (::d/mass entity)
                                                 (::d/position body)
                                                 (::d/mass body)
                                                 gravity-constant))]
    (reduce d/add-force entity forces)))

(defsystem step-body [::d/mass ::d/position ::d/velocity]
  [_ _ entity dt]
  (d/step-body entity dt))

(def init-state {::ecs/entities {#uuid "1e84d01d-369c-4dd0-a561-bcd9fec6c910"
                                 {::d/dimensions 2
                                  ::d/position [0 0.5]
                                  ::d/mass 1
                                  ::d/velocity [0.25 0]
                                  ::color [1 0 0]}
                                 #uuid "f7fb1804-441c-4e2d-bbc5-eb3fb3c18c45"
                                 {::d/dimensions 2
                                  ::d/position [-0.25 0]
                                  ::d/mass 10.2
                                  ::d/velocity [0 0]
                                  ::color [0 1 0]}
                                 #uuid "af72171d-bd1c-43ca-b507-7ad2acf9b9c1"
                                 {::d/dimensions 2
                                  ::d/position [0.5 0]
                                  ::d/mass 1.15
                                  ::d/velocity [0 0.1]
                                  ::color [0 0 1]}}
                 ::ecs/systems [[#'gravity #'step-body]]})
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
    (swap! state ecs/step-scene 0.1)
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
    gl_Position = vec4(vec3(objPos, 0) + vPos, 1) * vec4(0.3, 0.3, 0, 1);
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
