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
