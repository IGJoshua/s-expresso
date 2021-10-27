(ns s-expresso.texture
  "Functions for loading images and uploading them to the GPU."
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [s-expresso.math :as math]
   [s-expresso.memory :as m :refer [with-stack-allocator]]
   [s-expresso.resource :refer [Resource]])
  (:import
   (java.io
    File RandomAccessFile)
   (java.nio
    ByteBuffer)
   (java.nio.channels
    FileChannel FileChannel$MapMode)
   (org.lwjgl.opengl
    GL45)
   (org.lwjgl.stb
    STBImage)))

(s/def ::dimensions (s/tuple nat-int? nat-int?))
(s/def ::data (partial instance? ByteBuffer))
(s/def ::channels nat-int?)
(s/def ::original-channels nat-int?)

(defrecord Image [data dimensions channels]
  Resource
  (free [img]
    (STBImage/stbi_image_free ^ByteBuffer data)))

(defn load-image
  "Loads an image into a ByteBuffer, returning an [[Image]] [[Resource]].
  The image is attempted to be loaded from the resource bundle, then the path,
  then as a [[java.io.File]].

  If `desired-channels` is set, then an `:original-channels` key will be present
  in the result with the number of channels in the read file."
  ([file-or-path] (load-image file-or-path nil))
  ([file-or-path desired-channels]
   (with-open [file (RandomAccessFile. (if (string? file-or-path)
                                         (io/file (or (io/resource file-or-path)
                                                      file-or-path))
                                         ^File file-or-path)
                                       "r")]
     (let [mapped-buffer (.map (.getChannel file)
                               FileChannel$MapMode/READ_ONLY
                               0
                               (.length file))]
       (with-stack-allocator
         (let [width (m/alloc-bytes Integer/BYTES)
               height (m/alloc-bytes Integer/BYTES)
               channels (m/alloc-bytes Integer/BYTES)
               data (STBImage/stbi_load_from_memory
                     mapped-buffer
                     (.asIntBuffer
                      width)
                     (.asIntBuffer
                      height)
                     (.asIntBuffer
                      channels)
                     (int (or desired-channels 0)))
               width (.getInt width)
               height (.getInt height)
               channels (.getInt channels)
               ret {:data data
                    :dimensions [width height]
                    :channels (or desired-channels
                                  channels)}]
           (map->Image
            (if desired-channels
              (assoc ret :original-channels channels)
              ret))))))))
(s/fdef load-image
  :args (s/cat :file-or-path (s/or :file (partial instance? File)
                                   :path string?)
               :desired-channels (s/? nat-int?))
  :ret (partial instance? Image))

(def ^:private depth-stencil-mode->glenum
  {:depth-component GL45/GL_DEPTH_COMPONENT
   :stencil-index GL45/GL_STENCIL_INDEX})
(def depth-stencil-modes (set (keys depth-stencil-mode->glenum)))
(s/def :s-expresso.texture.parameter/depth-stencil-mode depth-stencil-modes)
(s/def :s-expresso.texture.parameter/base-level nat-int?)
(s/def :s-expresso.texture.parameter/border-color (s/coll-of float? :kind vector? :count 4))
(def ^:private compare-fn->glenum
  {:less-or-equal GL45/GL_LEQUAL
   :less GL45/GL_LESS
   :greater-or-equal GL45/GL_GEQUAL
   :greater GL45/GL_GREATER
   :equal GL45/GL_EQUAL
   :not-equal GL45/GL_NOTEQUAL
   :always GL45/GL_ALWAYS
   :never GL45/GL_NEVER})
(def compare-fns (set (keys compare-fn->glenum)))
(s/def :s-expresso.texture.parameter/compare-fn compare-fns)
(def ^:private compare-mode->glenum
  {:compare-ref-to-texture GL45/GL_COMPARE_REF_TO_TEXTURE
   :none GL45/GL_NONE})
(def compare-modes (set (keys compare-mode->glenum)))
(s/def :s-expresso.texture.parameter/compare-mode compare-modes)
(s/def :s-expresso.texture.parameter/lod-bias float?)
(def ^:private filter->glenum
  {:nearest GL45/GL_NEAREST
   :linear GL45/GL_LINEAR
   :nearest-mipmap-nearest GL45/GL_NEAREST_MIPMAP_NEAREST
   :linear-mipmap-nearest GL45/GL_LINEAR_MIPMAP_NEAREST
   :nearest-mipmap-lineaer GL45/GL_NEAREST_MIPMAP_LINEAR
   :linear-mipmap-linear GL45/GL_LINEAR_MIPMAP_LINEAR})
(def filters (set (keys filter->glenum)))
(s/def :s-expresso.texture.parameter/min-filter filters)
(s/def :s-expresso.texture.parameter/mag-filter #{:nearest :linear})
(s/def :s-expresso.texture.parameter/min-lod float?)
(s/def :s-expresso.texture.parameter/max-lod float?)
(s/def :s-expresso.texture.parameter/max-level int?)
(def ^:private swizzle->glenum
  {:red GL45/GL_RED
   :green GL45/GL_GREEN
   :blue GL45/GL_BLUE
   :alpha GL45/GL_ALPHA
   :zero GL45/GL_ZERO
   :one GL45/GL_ONE})
(def swizzles (set (keys swizzle->glenum)))
(s/def :s-expresso.texture.parameter/swizzle-r swizzles)
(s/def :s-expresso.texture.parameter/swizzle-g swizzles)
(s/def :s-expresso.texture.parameter/swizzle-b swizzles)
(s/def :s-expresso.texture.parameter/swizzle-a swizzles)
(s/def :s-expresso.texture.parameter/swizzle-rgba (s/coll-of swizzles :kind vector? :count 4))
(def ^:private wrap-mode->glenum
  {:clamp-to-edge GL45/GL_CLAMP_TO_EDGE
   :clamp-to-border GL45/GL_CLAMP_TO_BORDER
   :mirrored-repeat GL45/GL_MIRRORED_REPEAT
   :repeat GL45/GL_REPEAT
   :mirror-clamp-to-edge GL45/GL_MIRROR_CLAMP_TO_EDGE})
(def wrap-modes (set (keys wrap-mode->glenum)))
(s/def :s-expresso.texture.parameter/wrap-s wrap-modes)
(s/def :s-expresso.texture.parameter/wrap-t wrap-modes)
(s/def :s-expresso.texture.parameter/wrap-r wrap-modes)
(s/def ::tex-params (s/keys :opt-un [:s-expresso.texture.parameter/depth-stencil-mode
                                     :s-expresso.texture.parameter/base-level
                                     :s-expresso.texture.parameter/border-color
                                     :s-expresso.texture.parameter/compare-fn
                                     :s-expresso.texture.parameter/compare-mode
                                     :s-expresso.texture.parameter/lod-bias
                                     :s-expresso.texture.parameter/min-filter
                                     :s-expresso.texture.parameter/mag-filter
                                     :s-expresso.texture.parameter/min-lod
                                     :s-expresso.texture.parameter/max-lod
                                     :s-expresso.texture.parameter/max-level
                                     :s-expresso.texture.parameter/swizzle-r
                                     :s-expresso.texture.parameter/swizzle-g
                                     :s-expresso.texture.parameter/swizzle-b
                                     :s-expresso.texture.parameter/swizzle-a
                                     :s-expresso.texture.parameter/swizzle-rgba
                                     :s-expresso.texture.parameter/wrap-s
                                     :s-expresso.texture.parameter/wrap-t
                                     :s-expresso.texture.parameter/wrap-r]))

(def ^:private image-type->glenum
  {:tex-1d GL45/GL_TEXTURE_1D
   :tex-1d-array GL45/GL_TEXTURE_1D_ARRAY
   :tex-2d GL45/GL_TEXTURE_2D
   :tex-2d-array GL45/GL_TEXTURE_2D_ARRAY
   :tex-2d-multisample GL45/GL_TEXTURE_2D_MULTISAMPLE
   :tex-2d-multisample-array GL45/GL_TEXTURE_2D_MULTISAMPLE_ARRAY
   :tex-3d GL45/GL_TEXTURE_3D
   :tex-cube-map GL45/GL_TEXTURE_CUBE_MAP
   :tex-cube-map-array GL45/GL_TEXTURE_CUBE_MAP_ARRAY
   :tex-rectangle GL45/GL_TEXTURE_RECTANGLE})
(def image-types (set (keys image-type->glenum)))
(s/def ::type image-types)

(defrecord Texture [id tex-def opts]
  Resource
  (free [tex]
    (GL45/glDeleteTextures ^int id)))

(defn- apply-parameters
  "Applies a set of parameter options to a given texture."
  [tex-id opts]
  (when-let [stencil-mode (get opts :s-expresso.texture.parameter/depth-stencil-mode)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_DEPTH_STENCIL_TEXTURE_MODE
                              (depth-stencil-mode->glenum stencil-mode)))
  (when-let [base-level (get opts :s-expresso.texture.parameter/base-level)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_BASE_LEVEL
                              (int base-level)))
  (when-let [border-color (get opts :s-expresso.texture.parameter/border-color)]
    (GL45/glTextureParameterfv ^int tex-id
                               GL45/GL_TEXTURE_BORDER_COLOR
                               (float-array border-color)))
  (when-let [compare-fn (get opts :s-expresso.texture.parameter/compare-fn)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_COMPARE_FUNC
                              (compare-fn->glenum compare-fn)))
  (when-let [compare-mode (get opts :s-expresso.texture.parameter/compare-mode)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_COMPARE_MODE
                              (compare-mode->glenum compare-mode)))
  (when-let [lod-bias (get opts :s-expresso.texture.parameter/lod-bias)]
    (GL45/glTextureParameterf tex-id
                              GL45/GL_TEXTURE_LOD_BIAS
                              (float lod-bias)))
  (when-let [min-filter (get opts :s-expresso.texture.parameter/min-filter)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_MIN_FILTER
                              (filter->glenum min-filter)))
  (when-let [mag-filter (get opts :s-expresso.texture.parameter/mag-filter)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_MAG_FILTER
                              (filter->glenum mag-filter)))
  (when-let [min-lod (get opts :s-expresso.texture.parameter/min-lod)]
    (GL45/glTextureParameterf tex-id
                              GL45/GL_TEXTURE_MIN_LOD
                              (float min-lod)))
  (when-let [max-lod (get opts :s-expresso.texture.parameter/max-lod)]
    (GL45/glTextureParameterf tex-id
                              GL45/GL_TEXTURE_MAX_LOD
                              (float max-lod)))
  (when-let [max-level (get opts :s-expresso.texture.parameter/max-level)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_MAX_LEVEL
                              (int max-level)))
  (when-let [swizzle-r (get opts :s-expresso.texture.parameter/swizzle-r)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_SWIZZLE_R
                              (swizzle->glenum swizzle-r)))
  (when-let [swizzle-g (get opts :s-expresso.texture.parameter/swizzle-g)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_SWIZZLE_G
                              (swizzle->glenum swizzle-g)))
  (when-let [swizzle-b (get opts :s-expresso.texture.parameter/swizzle-b)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_SWIZZLE_B
                              (swizzle->glenum swizzle-b)))
  (when-let [swizzle-a (get opts :s-expresso.texture.parameter/swizzle-a)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_SWIZZLE_A
                              (swizzle->glenum swizzle-a)))
  (when-let [swizzle-rgba (get opts :s-expresso.texture.parameter/swizzle-rgba)]
    (GL45/glTextureParameteriv ^int tex-id
                               GL45/GL_TEXTURE_SWIZZLE_RGBA
                               (int-array (map swizzle->glenum swizzle-rgba))))
  (when-let [wrap-s (get opts :s-expresso.texture.parameter/wrap-s)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_WRAP_S
                              (wrap-mode->glenum wrap-s)))
  (when-let [wrap-t (get opts :s-expresso.texture.parameter/wrap-t)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_WRAP_T
                              (wrap-mode->glenum wrap-t)))
  (when-let [wrap-r (get opts :s-expresso.texture.parameter/wrap-r)]
    (GL45/glTextureParameteri tex-id
                              GL45/GL_TEXTURE_WRAP_R
                              (wrap-mode->glenum wrap-r)))
  nil)

(def ^:private image-internal-format->glenum
  {:r8 GL45/GL_R8
   :r8-snorm GL45/GL_R8_SNORM
   :r16 GL45/GL_R16
   :r16-snorm GL45/GL_R16_SNORM
   :rg8 GL45/GL_RG8
   :rg8-snorm GL45/GL_RG8_SNORM
   :rg16 GL45/GL_RG16
   :rg16-snorm GL45/GL_RG16_SNORM
   :r3-g3-b2 GL45/GL_R3_G3_B2
   :rgb4 GL45/GL_RGB4
   :rgb5 GL45/GL_RGB5
   :rgb8 GL45/GL_RGB8
   :rgb8-snorm GL45/GL_RGB8_SNORM
   :rgb10 GL45/GL_RGB10
   :rgb12 GL45/GL_RGB12
   :rgb16-snorm GL45/GL_RGB16_SNORM
   :rgba2 GL45/GL_RGBA2
   :rgba4 GL45/GL_RGBA4
   :rgb5-a1 GL45/GL_RGB5_A1
   :rgba8 GL45/GL_RGBA8
   :rgba8-snorm GL45/GL_RGBA8_SNORM
   :rgb10-a2 GL45/GL_RGB10_A2
   :rgb10-a2ui GL45/GL_RGB10_A2UI
   :rgba12 GL45/GL_RGBA12
   :rgba16 GL45/GL_RGBA16
   :srgb8 GL45/GL_SRGB8
   :srgb8-alpha8 GL45/GL_SRGB8_ALPHA8
   :r16f GL45/GL_R16F
   :rg16f GL45/GL_RG16F
   :rgb16f GL45/GL_RGB16F
   :rgba16f GL45/GL_RGBA16F
   :r32f GL45/GL_R32F
   :rg32f GL45/GL_RG32F
   :rgb32f GL45/GL_RGB32F
   :rgba32f GL45/GL_RGBA32F
   :r11f-g11f-b10f GL45/GL_R11F_G11F_B10F
   :rgb9-e5 GL45/GL_RGB9_E5
   :r8i GL45/GL_R8I
   :r8ui GL45/GL_R8UI
   :r16i GL45/GL_R16I
   :r16ui GL45/GL_R16UI
   :r32i GL45/GL_R32I
   :r32ui GL45/GL_R32UI
   :rg8i GL45/GL_RG8I
   :rg8ui GL45/GL_RG8UI
   :rg16i GL45/GL_RG16I
   :rg16ui GL45/GL_RG16UI
   :rg32i GL45/GL_RG32I
   :rg32ui GL45/GL_RG32UI
   :rgb8i GL45/GL_RGB8I
   :rgb8ui GL45/GL_RGB8UI
   :rgb16i GL45/GL_RGB16I
   :rgb16ui GL45/GL_RGB16UI
   :rgb32i GL45/GL_RGB32I
   :rgb32ui GL45/GL_RGB32UI
   :rgba8i GL45/GL_RGBA8I
   :rgba8ui GL45/GL_RGBA8UI
   :rgba16i GL45/GL_RGBA16I
   :rgba16ui GL45/GL_RGBA16UI
   :rgba32i GL45/GL_RGBA32I
   :rgba32ui GL45/GL_RGBA32UI})
(def image-internal-formats (set (keys image-internal-format->glenum)))
(s/def ::internal-format image-internal-format->glenum)
(s/def ::levels integer?)
(s/def ::texture-definition (s/keys :req-un [::dimensions ::internal-format]
                                    :opt-un [::levels]))

(def ^:private image-format->glenum
  {:red GL45/GL_RED
   :rg GL45/GL_RG
   :rgb GL45/GL_RGB
   :bgr GL45/GL_BGR
   :rgba GL45/GL_RGBA
   :bgra GL45/GL_BGRA
   :depth-component GL45/GL_DEPTH_COMPONENT
   :stencil-index GL45/GL_STENCIL_INDEX})
(def image-formats (set (keys image-format->glenum)))
(s/def ::format image-formats)
(def ^:private data-type->glenum
  {:unsigned-byte GL45/GL_UNSIGNED_BYTE
   :byte GL45/GL_BYTE
   :unsigned-short GL45/GL_UNSIGNED_SHORT
   :short GL45/GL_SHORT
   :unsigned-int GL45/GL_UNSIGNED_INT
   :int GL45/GL_INT
   :float GL45/GL_FLOAT
   :unsigned-byte-3-3-2 GL45/GL_UNSIGNED_BYTE_3_3_2
   :unsigned-byte-2-3-3-rev GL45/GL_UNSIGNED_BYTE_2_3_3_REV
   :unsigned-short-5-6-5 GL45/GL_UNSIGNED_SHORT_5_6_5
   :unsigned-short-5-6-5-rev GL45/GL_UNSIGNED_SHORT_5_6_5_REV
   :unsigned-short-4-4-4-4 GL45/GL_UNSIGNED_SHORT_4_4_4_4
   :unsigned-short-4-4-4-4-rev GL45/GL_UNSIGNED_SHORT_4_4_4_4_REV
   :unsigned-short-5-5-5-1 GL45/GL_UNSIGNED_SHORT_5_5_5_1
   :unsigned-short-1-5-5-5-rev GL45/GL_UNSIGNED_SHORT_1_5_5_5_REV
   :unsigned-int-8-8-8-8 GL45/GL_UNSIGNED_INT_8_8_8_8
   :unsigned-int-8-8-8-8-rev GL45/GL_UNSIGNED_INT_8_8_8_8_REV
   :unsigned-int-10-10-10-2 GL45/GL_UNSIGNED_INT_10_10_10_2
   :unsigned-int-2-10-10-10-rev GL45/GL_UNSIGNED_INT_2_10_10_10_REV})
(def data-types (set (keys data-type->glenum)))
(s/def ::data-type data-types)
(s/def ::texture-data (s/keys :req-un [::format ::data-type ::data]))

(defn make-texture
  "Creates a new texture from the given `tex-def`.
  The texture parameters from `opts` are applied before the image is uploaded."
  ([tex-def tex-data] (make-texture tex-def tex-data nil))
  ([tex-def tex-data opts]
   ;; TODO(Joshua): Support non-2d images
   (let [tex-id (GL45/glCreateTextures GL45/GL_TEXTURE_2D)]
     (when opts
       (apply-parameters tex-id opts))
     (let [width (int (nth (:dimensions tex-def) 0))
           height (int (nth (:dimensions tex-def) 1))]
       (GL45/glTextureStorage2D tex-id
                                (or (:levels tex-def)
                                    (int
                                     (min (math/log width 2)
                                          (math/log height 2))))
                                (image-internal-format->glenum (:internal-format tex-def))
                                width height)
       (GL45/glTextureSubImage2D tex-id 0 0 0
                                 width height
                                 ^int (image-format->glenum (:format tex-data))
                                 ^int (data-type->glenum (:data-type tex-data))
                                 ^ByteBuffer (:data tex-data)))
     (GL45/glGenerateTextureMipmap tex-id)
     (->Texture tex-id tex-def opts))))
(s/fdef make-texture
  :args (s/cat :tex-def ::texture-definition
               :tex-data ::texture-data
               :opts (s/? ::tex-params))
  :ret (partial instance? Texture))

(defn bind-texture
  "Binds a `texture` to the given `tex-unit`.
  Unbinds the texture unit if given nil."
  [texture tex-unit]
  (GL45/glBindTextureUnit tex-unit (or (:id texture) 0)))
(s/fdef bind-texture
  :args (s/cat :texture (partial instance? Texture))
  :ret nil?)

(defmacro with-texture
  "Binds the `texture` to `tex-unit` for the duration of the `body`.
  Whatever texture was bound before this call will be re-bound after it exits,
  even in the case an exception is thrown."
  [texture tex-unit & body]
  `(let [active-texture# (GL45/glGetInteger GL45/GL_ACTIVE_TEXTURE)
         _# (GL45/glActiveTexture (+ GL45/GL_TEXTURE0 ~tex-unit))
         old-texture# (GL45/glGetInteger GL45/GL_TEXTURE_BINDING_2D)
         _# (GL45/glActiveTexture active-texture#)
         texture# ~texture
         tex-unit# ~tex-unit]
     (bind-texture texture# tex-unit#)
     (try ~@body
          (finally (bind-texture old-texture# tex-unit#)))))
(s/fdef with-texture
  :args (s/cat :texture (s/or :symbol symbol?
                              :list list?)
               :body (s/* any?)))
