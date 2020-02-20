(ns s-expresso.shader
  "Functions to compile and link GLSL shader programs."
  (:require
   [s-expresso.resource :refer [Resource free]]
   [taoensso.timbre :as log])
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

(defn compile-shader
  "Compiles a shader, returning a shader object.
  Takes a `source` and a `stage` and returns a [[Shader]] record which has been
  compiled. If the shader is invalid, the compilation error is logged nil is
  returned."
  [source stage]
  (let [id (GL45/glCreateShader (shader-stage->shader-stage-int stage))
        status (int-array 1)]
    (GL45/glShaderSource id ^CharSequence source)
    (GL45/glCompileShader id)
    (GL45/glGetShaderiv id GL45/GL_COMPILE_STATUS status)
    (if (zero? (first status))
      (let [info-log (GL45/glGetShaderInfoLog id)]
        (log/errorf "Shader stage %s failed to compile with message: %s\n%s" (str stage) info-log source)
        (GL45/glDeleteShader id)
        nil)
      (->Shader id source stage))))

(defn link-shader-program
  "Links shaders into a shader program.
  Takes a seq of [[Shader]]s which must contain either a vertex shader (and
  optionally other shaders, but not compute), or exactly one compute shader. If
  a linker error occurs it will be displayed, along with the whole program text
  in pipeline order, each stage separated by three newlines, and nil will be
  returned."
  [shaders]
  {:pre [(distinct? (map :stage shaders))
         (every? shader-stages (map :stage shaders))
         (or (and (:vertex (set (map :stage shaders)))
                  (not (:compute (set (map :stage shaders)))))
             (and (= 1 (count shaders))
                  (:compute (set (map :stage shaders)))))]}
  (let [shaders (sort-by (comp shader-stage->ordering :stage) shaders)
        program (GL45/glCreateProgram)]
    (doseq [shader shaders]
      (GL45/glAttachShader program (:id shader)))
    (GL45/glLinkProgram program)
    (let [status (int-array 1)]
      (GL45/glGetProgramiv program GL45/GL_LINK_STATUS status)
      (when (zero? (first status))
        (let [info-log (GL45/glGetProgramInfoLog program)]
          (log/errorf "Shader program failed to link with message: %s\n%s"
                      info-log
                      (apply str (interpose "\n\n\n" (map :source shaders))))
          (GL45/glDeleteProgram program)
          nil)))
    (->ShaderProgram program shaders)))

(defn make-shader-program-from-sources
  "Compiles the given sources as shaders and links them, returning the program.
  All of the shaders are released to the driver after linking, meaning when the
  program is freed, the shaders will be as well.

  If a compilation error occurs in any shader, all shaders will still be
  compiled and all errors displayed, and if all compile successfully but a link
  error occurs the error will again be displayed, and nil will be returned in
  either case."
  [shader-sources]
  (let [shaders (map (comp (partial apply compile-shader)
                           (juxt :source :stage))
                     shader-sources)
        program (if (every? some? shaders)
                  (link-shader-program shaders)
                  nil)]
    (run! free shaders)
    program))
