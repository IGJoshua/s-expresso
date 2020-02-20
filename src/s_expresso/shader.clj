(ns s-expresso.shader
  "Functions to compile and link GLSL shader programs."
  (:require
   [s-expresso.resource :refer [Resource]])
  (:import
   (org.lwjgl.opengl
    GL45)))

(defrecord ShaderProgram [id shaders]
  Resource
  (free [prog]
    (when id
      (GL45/glDeleteProgram id))))

(defrecord Shader [id source stage]
  Resource
  (free [shader]
    (when id
      (GL45/glDeleteShader id))))

(def ^:private shader-stage->shader-stage-int
  "Map from keyword shader stages to the GL int to pass."
  {:vertex GL45/GL_VERTEX_SHADER
   :tess-control GL45/GL_TESS_CONTROL_SHADER
   :tess-eval GL45/GL_TESS_EVALUATION_SHADER
   :geometry GL45/GL_GEOMETRY_SHADER
   :fragment GL45/GL_FRAGMENT_SHADER
   :compute GL45/GL_COMPUTE_SHADER})

(def shader-stages
  "Set of all valid shader stages."
  (set (keys shader-stage->shader-stage-int)))

(def ^:private shader-stage->ordering
  "Map from the shader stage to an integer representing the ordering."
  {:compute -500
   :vertex 1
   :tess-control 2
   :tess-eval 3
   :geometry 4
   :fragment 5})
