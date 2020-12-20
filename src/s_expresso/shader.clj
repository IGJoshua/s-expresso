(ns s-expresso.shader
  "Functions to compile and link GLSL shader programs."
  (:require
   [s-expresso.memory :as m :refer [with-stack-allocator]]
   [s-expresso.resource :refer [Resource free]]
   [taoensso.timbre :as log])
  (:import
   (java.nio
    DoubleBuffer FloatBuffer IntBuffer)
   (org.lwjgl.opengl
    GL45)))

(defrecord ShaderProgram [id shaders uniforms]
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

(def ^:private glenum->uniform-type
  "Map from glenum to the uniform type keyword."
  {GL45/GL_FLOAT :float
   GL45/GL_FLOAT_VEC2 :vec2
   GL45/GL_FLOAT_VEC3 :vec3
   GL45/GL_FLOAT_VEC4 :vec4
   GL45/GL_DOUBLE :double
   GL45/GL_DOUBLE_VEC2 :dvec2
   GL45/GL_DOUBLE_VEC3 :dvec3
   GL45/GL_DOUBLE_VEC4 :dvec4
   GL45/GL_INT :int
   GL45/GL_INT_VEC2 :ivec2
   GL45/GL_INT_VEC3 :ivec3
   GL45/GL_INT_VEC4 :ivec4
   GL45/GL_UNSIGNED_INT :int
   GL45/GL_UNSIGNED_INT_VEC2 :uvec2
   GL45/GL_UNSIGNED_INT_VEC3 :uvec3
   GL45/GL_UNSIGNED_INT_VEC4 :uvec4
   GL45/GL_BOOL :bool
   GL45/GL_BOOL_VEC2 :bvec2
   GL45/GL_BOOL_VEC3 :bvec3
   GL45/GL_BOOL_VEC4 :bvec4
   GL45/GL_FLOAT_MAT2 :mat2
   GL45/GL_FLOAT_MAT3 :mat3
   GL45/GL_FLOAT_MAT4 :mat4
   GL45/GL_FLOAT_MAT2x3 :mat2x3
   GL45/GL_FLOAT_MAT2x4 :mat2x4
   GL45/GL_FLOAT_MAT3x2 :mat3x2
   GL45/GL_FLOAT_MAT3x4 :mat3x4
   GL45/GL_FLOAT_MAT4x2 :mat4x2
   GL45/GL_FLOAT_MAT4x3 :mat4x3
   GL45/GL_DOUBLE_MAT2 :dmat2
   GL45/GL_DOUBLE_MAT3 :dmat3
   GL45/GL_DOUBLE_MAT4 :dmat4
   GL45/GL_DOUBLE_MAT2x3 :dmat2x3
   GL45/GL_DOUBLE_MAT2x4 :dmat2x4
   GL45/GL_DOUBLE_MAT3x2 :dmat3x2
   GL45/GL_DOUBLE_MAT3x4 :dmat3x4
   GL45/GL_DOUBLE_MAT4x2 :dmat4x2
   GL45/GL_DOUBLE_MAT4x3 :dmat4x3
   GL45/GL_SAMPLER_1D :sampler1d
   GL45/GL_SAMPLER_2D :sampler2d
   GL45/GL_SAMPLER_3D :sampler3d
   GL45/GL_SAMPLER_CUBE :sampler-cube
   GL45/GL_SAMPLER_1D_SHADOW :sampler1d-shadow
   GL45/GL_SAMPLER_2D_SHADOW :sampler2d-shadow
   GL45/GL_SAMPLER_1D_ARRAY :sampler1d-array
   GL45/GL_SAMPLER_2D_ARRAY :sampler2d-array
   GL45/GL_SAMPLER_1D_ARRAY_SHADOW :sampler1d-array-shadow
   GL45/GL_SAMPLER_2D_ARRAY_SHADOW :sampler2d-array-shadow
   GL45/GL_SAMPLER_2D_MULTISAMPLE :sampler2d-multisample
   GL45/GL_SAMPLER_2D_MULTISAMPLE_ARRAY :sampler2d-multisample-array
   GL45/GL_SAMPLER_CUBE_SHADOW :sampler-cube-shadow
   GL45/GL_SAMPLER_BUFFER :sampler-buffer
   GL45/GL_SAMPLER_2D_RECT :sampler2d-rect
   GL45/GL_SAMPLER_2D_RECT_SHADOW :sampler2d-rect-shadow
   GL45/GL_INT_SAMPLER_1D :isampler1d
   GL45/GL_INT_SAMPLER_2D :isampler2d
   GL45/GL_INT_SAMPLER_3D :isampler3d
   GL45/GL_INT_SAMPLER_CUBE :isampler-cube
   GL45/GL_INT_SAMPLER_1D_ARRAY :isampler1d-array
   GL45/GL_INT_SAMPLER_2D_ARRAY :isampler2d-array
   GL45/GL_INT_SAMPLER_2D_MULTISAMPLE :isampler2d-multisample
   GL45/GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY :isampler2d-multisample-array
   GL45/GL_INT_SAMPLER_BUFFER :isampler-buffer
   GL45/GL_INT_SAMPLER_2D_RECT :isampler2d-rect
   GL45/GL_UNSIGNED_INT_SAMPLER_1D :usampler1d
   GL45/GL_UNSIGNED_INT_SAMPLER_2D :usampler2d
   GL45/GL_UNSIGNED_INT_SAMPLER_3D :usampler3d
   GL45/GL_UNSIGNED_INT_SAMPLER_CUBE :usampler-cube
   GL45/GL_UNSIGNED_INT_SAMPLER_1D_ARRAY :usampler1d-array
   GL45/GL_UNSIGNED_INT_SAMPLER_2D_ARRAY :usampler2d-array
   GL45/GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE :usampler2d-multisample
   GL45/GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY :usampler2d-multisample-array
   GL45/GL_UNSIGNED_INT_SAMPLER_BUFFER :usampler-buffer
   GL45/GL_UNSIGNED_INT_SAMPLER_2D_RECT :usampler2d-rect
   GL45/GL_IMAGE_1D :image1d
   GL45/GL_IMAGE_2D :image2d
   GL45/GL_IMAGE_3D :image3d
   GL45/GL_IMAGE_2D_RECT :image2d-rect
   GL45/GL_IMAGE_CUBE :image-cube
   GL45/GL_IMAGE_BUFFER :image-buffer
   GL45/GL_IMAGE_1D_ARRAY :image1d-array
   GL45/GL_IMAGE_2D_ARRAY :image2d-array
   GL45/GL_IMAGE_2D_MULTISAMPLE :image2d-multisample
   GL45/GL_IMAGE_2D_MULTISAMPLE_ARRAY :image2d-multisample-array
   GL45/GL_INT_IMAGE_1D :iimage1d
   GL45/GL_INT_IMAGE_2D :iimage2d
   GL45/GL_INT_IMAGE_3D :iimage3d
   GL45/GL_INT_IMAGE_2D_RECT :iimage2d-rect
   GL45/GL_INT_IMAGE_CUBE :iimage-cube
   GL45/GL_INT_IMAGE_BUFFER :iimage-buffer
   GL45/GL_INT_IMAGE_1D_ARRAY :iimage1d-array
   GL45/GL_INT_IMAGE_2D_ARRAY :iimage2d-array
   GL45/GL_INT_IMAGE_2D_MULTISAMPLE :iimage2d-multisample
   GL45/GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY :iimage2d-multisample-array
   GL45/GL_UNSIGNED_INT_IMAGE_1D :uimage1d
   GL45/GL_UNSIGNED_INT_IMAGE_2D :uimage2d
   GL45/GL_UNSIGNED_INT_IMAGE_3D :uimage3d
   GL45/GL_UNSIGNED_INT_IMAGE_2D_RECT :uimage2d-rect
   GL45/GL_UNSIGNED_INT_IMAGE_CUBE :uimage-cube
   GL45/GL_UNSIGNED_INT_IMAGE_BUFFER :uimage-buffer
   GL45/GL_UNSIGNED_INT_IMAGE_1D_ARRAY :uimage1d-array
   GL45/GL_UNSIGNED_INT_IMAGE_2D_ARRAY :uimage2d-array
   GL45/GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE :uimage2d-multisample
   GL45/GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY :uimage2d-multisample-array
   GL45/GL_UNSIGNED_INT_ATOMIC_COUNTER :atomic-uint})

(def uniform-types (set (vals glenum->uniform-type)))

(defn uniform-map
  "Fetches all active uniforms from a shader `program`.
  Returns a map from the string name of the uniform to its uniform location in
  the shader program."
  [program]
  (let [id (:id program)
        num-uniforms (with-stack-allocator
                       (let [num-uniforms (m/alloc-bytes Integer/BYTES)]
                         (GL45/glGetProgramiv ^int id GL45/GL_ACTIVE_UNIFORMS
                                              ^IntBuffer (.asIntBuffer
                                                          num-uniforms))
                         (.getInt num-uniforms)))]
    (reduce (fn [uniforms idx]
              (with-stack-allocator
                (let [array-size (m/alloc-bytes Integer/BYTES)
                      uniform-type (m/alloc-bytes Integer/BYTES)
                      uniform-name (GL45/glGetActiveUniform id idx
                                                            (.asIntBuffer
                                                             array-size)
                                                            (.asIntBuffer
                                                             uniform-type))]
                  (assoc uniforms uniform-name {:location idx
                                                :type (glenum->uniform-type (.getInt uniform-type))
                                                :count (.getInt array-size)}))))
            {}
            (range num-uniforms))))

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
    (let [prog (->ShaderProgram program shaders nil)]
      (assoc prog :uniforms (uniform-map prog)))))

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

(defn bind-shader-program
  "Takes a shader `program` and binds it for drawing.
  Unbinds the program if given nil."
  [program]
  (GL45/glUseProgram (if (:id program) (:id program) 0)))

(defn upload-uniform-float
  "Uploads floating-point values to a uniform.
  The floating-point values are assumed to be vectors when 2-4 values are
  passed."
  ([program uniform-name x]
   (GL45/glProgramUniform1f (:id program) (:location (get (:uniforms program) uniform-name)) x))
  ([program uniform-name x y]
   (GL45/glProgramUniform2f (:id program) (:location (get (:uniforms program) uniform-name)) x y))
  ([program uniform-name x y z]
   (GL45/glProgramUniform3f (:id program) (:location (get (:uniforms program) uniform-name)) x y z))
  ([program uniform-name x y z w]
   (GL45/glProgramUniform4f (:id program) (:location (get (:uniforms program) uniform-name)) x y z w)))

(defn upload-uniform-floats
  "Uploads floating-point values to a uniform array.
  `floats` must be a [[java.nio.FloatBuffer]]."
  [program num-components uniform-name ^FloatBuffer floats]
  (case (int num-components)
    1 (GL45/glProgramUniform1fv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                floats)
    2 (GL45/glProgramUniform2fv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                floats)
    3 (GL45/glProgramUniform3fv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                floats)
    4 (GL45/glProgramUniform4fv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                floats)))

(defn upload-uniform-int
  "Uploads integral values to a uniform.
  The integral values are assumed to be vectors when 2-4 values are passed."
  ([program uniform-name x]
   (GL45/glProgramUniform1i (:id program) (:location (get (:uniforms program) uniform-name)) x))
  ([program uniform-name x y]
   (GL45/glProgramUniform2i (:id program) (:location (get (:uniforms program) uniform-name)) x y))
  ([program uniform-name x y z]
   (GL45/glProgramUniform3i (:id program) (:location (get (:uniforms program) uniform-name)) x y z))
  ([program uniform-name x y z w]
   (GL45/glProgramUniform4i (:id program) (:location (get (:uniforms program) uniform-name)) x y z w)))

(defn upload-uniform-ints
  "Uploads integral values to a uniform array.
  `ints` must be a [[java.nio.IntBuffer]]."
  [program num-components uniform-name ^IntBuffer ints]
  (case (int num-components)
    1 (GL45/glProgramUniform1iv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                ints)
    2 (GL45/glProgramUniform2iv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                ints)
    3 (GL45/glProgramUniform3iv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                ints)
    4 (GL45/glProgramUniform4iv ^int (:id program)
                                ^int (:location (get (:uniforms program) uniform-name))
                                ints)))

(defn upload-uniform-unsigned-int
  "Uploads unsigned integral values to a uniform.
  The unsigned integral values are assumed to be vectors when 2-4 values are
  passed."
  ([program uniform-name x]
   (GL45/glProgramUniform1ui (:id program) (:location (get (:uniforms program) uniform-name)) x))
  ([program uniform-name x y]
   (GL45/glProgramUniform2ui (:id program) (:location (get (:uniforms program) uniform-name)) x y))
  ([program uniform-name x y z]
   (GL45/glProgramUniform3ui (:id program) (:location (get (:uniforms program) uniform-name)) x y z))
  ([program uniform-name x y z w]
   (GL45/glProgramUniform4ui (:id program) (:location (get (:uniforms program) uniform-name)) x y z w)))

(defn upload-uniform-ints
  "Uploads unsigned integral values to a uniform array.
  `ints` must be a [[java.nio.IntBuffer]]."
  [program num-components uniform-name ^IntBuffer ints]
  (case (int num-components)
    1 (GL45/glProgramUniform1uiv ^int (:id program)
                                 ^int (:location (get (:uniforms program) uniform-name))
                                 ints)
    2 (GL45/glProgramUniform2uiv ^int (:id program)
                                 ^int (:location (get (:uniforms program) uniform-name))
                                 ints)
    3 (GL45/glProgramUniform3uiv ^int (:id program)
                                 ^int (:location (get (:uniforms program) uniform-name))
                                 ints)
    4 (GL45/glProgramUniform4uiv ^int (:id program)
                                 ^int (:location (get (:uniforms program) uniform-name))
                                 ints)))

(defn upload-uniform-matrix2
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix2 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix2fv ^int (:id program)
                                   ^int (:location (get (:uniforms program) uniform-name))
                                   ^boolean transpose?
                                   ^FloatBuffer matrix)))

(defn upload-uniform-matrix2x3
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix2x3 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix2x3fv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^FloatBuffer matrix)))

(defn upload-uniform-matrix2x4
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix2x4 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix2x4fv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^FloatBuffer matrix)))

(defn upload-uniform-matrix3
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix3 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix3fv ^int (:id program)
                                   ^int (:location (get (:uniforms program) uniform-name))
                                   ^boolean transpose?
                                   ^FloatBuffer matrix)))

(defn upload-uniform-matrix3x2
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix3x2 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix3x2fv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^FloatBuffer matrix)))

(defn upload-uniform-matrix3x4
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix3x4 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix3x4fv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^FloatBuffer matrix)))

(defn upload-uniform-matrix4
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix4 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix4fv ^int (:id program)
                                   ^int (:location (get (:uniforms program) uniform-name))
                                   ^boolean transpose?
                                   ^FloatBuffer matrix)))

(defn upload-uniform-matrix4x2
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix4x2 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix4x2fv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^FloatBuffer matrix)))
(defn upload-uniform-matrix4x3
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.FloatBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-matrix4x3 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix4x3fv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^FloatBuffer matrix)))

(defn upload-uniform-dmatrix2
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix2 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix2dv ^int (:id program)
                                   ^int (:location (get (:uniforms program) uniform-name))
                                   ^boolean transpose?
                                   ^DoubleBuffer matrix)))

(defn upload-uniform-dmatrix2x3
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix2x3 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix2x3dv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^DoubleBuffer matrix)))

(defn upload-uniform-dmatrix2x4
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix2x4 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix2x4dv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^DoubleBuffer matrix)))

(defn upload-uniform-dmatrix3
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix3 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix3dv ^int (:id program)
                                   ^int (:location (get (:uniforms program) uniform-name))
                                   ^boolean transpose?
                                   ^DoubleBuffer matrix)))

(defn upload-uniform-dmatrix3x2
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix3x2 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix3x2dv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^DoubleBuffer matrix)))

(defn upload-uniform-dmatrix3x4
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix3x4 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix3x4dv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^DoubleBuffer matrix)))

(defn upload-uniform-dmatrix4
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix4 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix4dv ^int (:id program)
                                   ^int (:location (get (:uniforms program) uniform-name))
                                   ^boolean transpose?
                                   ^DoubleBuffer matrix)))

(defn upload-uniform-dmatrix4x2
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix4x2 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix4x2dv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^DoubleBuffer matrix)))
(defn upload-uniform-dmatrix4x3
  "Uploads a floating-point matrix into a uniform array.
  `matrix` must be a [[java.nio.DoubleBuffer]]."
  ([program uniform-name matrix]
   (upload-uniform-dmatrix4x3 program uniform-name matrix false))
  ([program uniform-name matrix transpose?]
   (GL45/glProgramUniformMatrix4x3dv ^int (:id program)
                                     ^int (:location (get (:uniforms program) uniform-name))
                                     ^boolean transpose?
                                     ^DoubleBuffer matrix)))
