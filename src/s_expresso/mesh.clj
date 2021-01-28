(ns s-expresso.mesh
  "Functions for dealing with mesh data and buffer layouts."
  (:require
   [clojure.spec.alpha :as s]
   [s-expresso.memory :refer [alloc-bytes put put-seq with-heap-allocator]]
   [s-expresso.resource :refer [Resource]])
  (:import
   (java.nio ByteBuffer Buffer)
   (org.lwjgl.opengl
    GL45)))

(defn- float->half-float
  "Takes a floating point value and coerces it into a half-precision float."
  [fval]
  (let [fbits (Float/floatToIntBits fval)
        sign (bit-and (unsigned-bit-shift-right fbits 16)
                      0x8000)
        value (+ (bit-and fbits 0x7fffffff)
                 0x1000)]
    (if (>= value 0x47800000)
      (if (>= (bit-and fbits 0x7fffffff) 0x47800000)
        (if (< value 0x7f800000)
          (bit-or sign 0x7c00)
          (bit-or sign 0x7c00
                  (unsigned-bit-shift-right
                   (bit-and fbits 0x007fffff)
                   13)))
        (bit-or sign 0x7bff))
      (if (>= value 0x38800000)
        (bit-or sign (unsigned-bit-shift-right
                      (- value 0x38000000)
                      13))
        (if (< value 0x33000000)
          sign
          (let [value (unsigned-bit-shift-right
                       (bit-and fbits 0x7fffffff)
                       23)]
            (bit-or sign
                    (unsigned-bit-shift-right
                     (+ (bit-or (bit-and fbits 0x7fffff)
                                0x800000)
                        (unsigned-bit-shift-right
                         0x800000
                         (- value 102)))
                     (- 126 value)))))))))

(defn- half-float->float
  "Takes a half-precision float and coerces it into a floating point value."
  [hbits]
  (letfn [(combine [exp mant]
            (Float/intBitsToFloat
             (unchecked-int
              (bit-or (bit-shift-left
                       (bit-and hbits 0x8000)
                       16)
                      (bit-shift-left
                       (bit-or exp mant)
                       13)))))
          (normalize [exp mant]
            (if-not (zero? mant)
              (let [exp 0x1c400]
                (loop [mant (bit-shift-left mant 1)
                       exp (- exp 0x400)]
                  (if-not (zero? (bit-and mant 0x400))
                    [exp (bit-and mant 0x3ff)]
                    (recur (bit-shift-left mant 1)
                           (- exp 0x400)))))
              [exp mant]))]
    (let [mant (bit-and hbits 0x03ff)
          exp (bit-and hbits 0x7c00)]
      (if (= exp 0x7c00)
        (combine 0x3fc00 mant)
        (if-not (zero? exp)
          (let [exp (+ exp 0x1c000)]
            (if (and (zero? mant)
                     (> exp 0x1c400))
              (Float/intBitsToFloat
               (unchecked-int
                (bit-or
                 (bit-shift-left
                  (bit-and hbits 0x8000)
                  16)
                 (bit-shift-left exp 13)
                 0x3ff)))
              (combine exp mant)))
          (apply combine (normalize exp mant)))))))

(s/def ::vertex map?)
(s/def ::vertices (s/coll-of ::vertex :kind vector?))
(s/def ::indices (s/coll-of nat-int? :kind vector?))
(s/def ::mesh-data (s/keys :req-un [::vertices]
                           :opt-un [::indices]))

(def ^:private attrib-type->glenum
  "Map from an attribute type to the GLEnum value for that type."
  {:half-float GL45/GL_HALF_FLOAT
   :float GL45/GL_FLOAT
   :double GL45/GL_DOUBLE
   :byte GL45/GL_BYTE
   :short GL45/GL_SHORT
   :int GL45/GL_INT
   :ubyte GL45/GL_UNSIGNED_BYTE
   :ushort GL45/GL_UNSIGNED_SHORT
   :uint GL45/GL_UNSIGNED_INT})
(def ^:private attrib-type->size-in-bytes
  "Map from an attribute type to the size of that type in bytes."
  {:half-float 2
   :float 4
   :double 8
   :byte 1
   :short 2
   :int 4
   :ubyte 1
   :ushort 2
   :uint 4})
(def ^:private attrib-type->coersion-fn
  "Map from an attribute type to a coersion function used to create the value."
  {:half-float (comp unchecked-short float->half-float float)
   :float float
   :double double
   :byte byte
   :short short
   :int int
   :ubyte unchecked-byte
   :ushort unchecked-short
   :uint unchecked-int})
(def attrib-types (set (keys attrib-type->glenum)))
(s/def ::type attrib-types)
(s/def ::count pos-int?)
(s/def ::name keyword?)
(s/def ::convert-fn (s/fspec :args ::vertex
                             :ret (s/coll-of number? :kind vector?)))
(s/def ::normalized boolean?)
(s/def ::stride pos-int?)
(s/def ::attrib-layout (s/keys :req-un [::name ::type ::count]
                               :opt-un [::convert-fn ::normalized ::stride]))
(s/def ::attrib-layouts (s/coll-of ::attrib-layout :kind vector?))
(s/def ::interleaved boolean?)

(def ^:private usage-flag->glenum
  "Map from a usage flag to the glenum value which represents it."
  {:dynamic-storage GL45/GL_DYNAMIC_STORAGE_BIT
   :read GL45/GL_MAP_READ_BIT
   :write GL45/GL_MAP_WRITE_BIT
   :persistent GL45/GL_MAP_PERSISTENT_BIT
   :coherent GL45/GL_MAP_COHERENT_BIT
   :client-storage GL45/GL_CLIENT_STORAGE_BIT})
(def ^:private usage-flags
  "Set of all possible usage flags for a given buffer."
  (set (keys usage-flag->glenum)))

(defn- usage-flags->flags-int
  "Takes a set of usage `flags` and converts it to a bitset represented as an int.
  Integer values of individual `flags` are fetched from [[usage-flag->glenum]]
  and [[bit-or]]ed together to create a bitset."
  ^long [flags]
  (if (zero? (count flags))
    0
    (if (< (count flags) 2)
      (usage-flag->glenum (first flags))
      (apply bit-or (map usage-flag->glenum flags)))))

(s/def ::usage-flag usage-flags)
(s/def ::usage-flags (s/coll-of ::usage-flag :kind set?))
(s/def ::buffer-layout (s/and (s/keys :req-un [::attrib-layouts]
                                      :opt-un [::interleaved ::usage-flags])
                              (fn [layout]
                                (if (:interleaved layout)
                                  (apply = (map #(or (:stride %)
                                                     (* (attrib-type->size-in-bytes (:type %))
                                                        (:count %)))
                                                (:attrib-layouts layout)))
                                  true))))
(s/def ::buffer-layouts (s/coll-of ::buffer-layout :kind vector?))
(s/def :s-expresso.mesh.layout/indices (s/nilable
                                        (s/keys :opt-un [::usage-flags ::type])))

(def ^:private element-type->glenum
  "Map from an element type to the glenum value used in rendering that type."
  {:points GL45/GL_POINTS
   :lines GL45/GL_LINES
   :lines-adjacency GL45/GL_LINES_ADJACENCY
   :line-loop GL45/GL_LINE_LOOP
   :line-strip GL45/GL_LINE_STRIP
   :line-strip-adjacency GL45/GL_LINE_STRIP_ADJACENCY
   :triangles GL45/GL_TRIANGLES
   :triangles-adjacency GL45/GL_TRIANGLES_ADJACENCY
   :triangle-fan GL45/GL_TRIANGLE_FAN
   :triangle-strip GL45/GL_TRIANGLE_STRIP
   :triangle-strip-adjacency GL45/GL_TRIANGLE_STRIP_ADJACENCY
   :patches GL45/GL_PATCHES})
(def element-types (set (keys element-type->glenum)))
(s/def ::element-type element-types)
(s/def ::mesh-layout (s/keys :req-un [::buffer-layouts ::element-type]
                             :opt-un [:s-expresso.mesh.layout/indices]))

(defrecord Mesh [^int vao-id ^ByteBuffer buffers index-type element-count element-type start-offset]
  Resource
  (free [mesh]
    (GL45/glDeleteBuffers (.asIntBuffer buffers))
    (GL45/glDeleteVertexArrays vao-id)))

(s/def :s-expresso.mesh.packed/buffers (s/coll-of (partial instance? Buffer)))
(s/def :s-expresso.mesh.packed/indices (partial instance? Buffer))
(s/def :s-expresso.mesh.packed/element-count nat-int?)
(s/def :s-expresso.mesh.packed/offset nat-int?)
(s/def ::packed-mesh (s/keys :req-un [:s-expresso.mesh.packed/buffers
                                      :s-expresso.mesh.packed/element-count
                                      :s-expresso.mesh.packed/offset]
                             :opt-un [:s-expresso.mesh.packed/indices]))

(defn pack-verts
  "Takes in a buffer `layout` definition and a `mesh`, and returns packed buffers.
  Return value is a map with the keys `:indices` (if an index buffer is defined)
  and `:buffers`. `:indices` will have a value of an int array, and `:buffers`
  will be a vector of arrays based on the type specified in the `layout`.

  If inside a [[s-expresso.memory/with-stack-allocator]] call, the buffers will
  be allocated on the stack and will become invalid as soon as the call is
  complete."
  [layout mesh]
  (let [index-type (or (:type (:indices layout))
                       :uint)
        indices (when (:indices layout)
                  (doto ^ByteBuffer (put-seq (alloc-bytes (* (attrib-type->size-in-bytes index-type)
                                                             (count (:indices mesh))))
                                             (map (attrib-type->coersion-fn index-type) (:indices mesh)))
                    (.flip)))
        vert-count (count (:vertices mesh))
        buffers (vec
                 (for [buffer (:buffer-layouts layout)]
                   (let [stride (reduce (fn [acc v]
                                          (+ acc (* (attrib-type->size-in-bytes (:type v))
                                                    (:count v))))
                                        0 (:attrib-layouts buffer))
                         mem-buf (alloc-bytes (* stride vert-count))]
                     (if (:interleaved buffer)
                       (loop [verts (:vertices mesh)]
                         (let [vert (first verts)]
                           (doseq [{:keys [name type count convert-fn]} (:attrib-layouts buffer)]
                             (let [v (map (attrib-type->coersion-fn type)
                                          (take count (if convert-fn
                                                        (convert-fn vert)
                                                        (name vert))))]
                               (put-seq mem-buf v))))
                         (when (seq (rest verts))
                           (recur (rest verts))))
                       (doseq [{:keys [name type count convert-fn]} (:attrib-layouts buffer)]
                         (transduce (comp (map (if convert-fn convert-fn name))
                                          (map #(take count %))
                                          cat
                                          (map (attrib-type->coersion-fn type)))
                                    (completing
                                     (fn [buf v]
                                       (put v buf)
                                       buf))
                                    mem-buf
                                    (:vertices mesh))))
                     (.flip mem-buf)
                     mem-buf)))
        ret {:buffers buffers
             :element-count (int (if (:indices mesh)
                                   (count (:indices mesh))
                                   vert-count))
             :offset (int 0)}]
    (if indices
      (assoc ret :indices indices)
      ret)))
(s/fdef pack-verts
  :args (s/cat :layout ::mesh-layout
               :mesh ::mesh-data)
  :ret ::packed-mesh)

(defn make-mesh
  "Takes a `layout` and a `packed-mesh`, and returns a [[Mesh]] [[Resource]].
  The `layout` must match the `packed-mesh`. This function asserts that the
  number of buffers the `layout` expects matches the number of buffers in the
  `packed-mesh`, and that if one has indices, both do."
  [layout packed-mesh]
  {:pre [(= (count (:buffer-layouts layout))
            (count (:buffers packed-mesh)))
         (or (and (:indices layout)
                  (:indices packed-mesh))
             (and (not (:indices layout))
                  (not (:indices packed-mesh))))]}
  (let [vao (GL45/glCreateVertexArrays)
        ^ByteBuffer buffers (with-heap-allocator
                  (alloc-bytes (* (+ (if (:indices packed-mesh) 1 0)
                                     (count (:buffers packed-mesh)))
                                  Integer/BYTES)))
        attrib-idx (volatile! 0)]
    (when (:indices packed-mesh)
      (let [idx-buffer (GL45/glCreateBuffers)]
        (put (int idx-buffer) buffers)
        (GL45/glNamedBufferStorage idx-buffer ^ByteBuffer (:indices packed-mesh)
                                   (usage-flags->flags-int
                                    (or (:usage-flags (:indices layout))
                                        #{})))
        (GL45/glVertexArrayElementBuffer vao idx-buffer)))
    (doseq [[idx buffer-layout buffer] (map vector
                                            (range (count (:buffers packed-mesh)))
                                            (:buffer-layouts layout)
                                            (:buffers packed-mesh))]
      (let [buffer-array (GL45/glCreateBuffers)
            stride (if (:interleaved buffer-layout)
                     (reduce (fn [acc v]
                               (+ acc (* (attrib-type->size-in-bytes (:type v))
                                         (:count v))))
                             0 (:attrib-layouts buffer-layout))
                     (let [v (first (:attrib-layouts buffer-layout))]
                       (* (attrib-type->size-in-bytes (:type v))
                          (:count v))))]
        (put (int buffer-array) buffers)
        (GL45/glNamedBufferStorage buffer-array ^ByteBuffer buffer
                                   (usage-flags->flags-int (:usage-flags buffer-layout)))
        (GL45/glVertexArrayVertexBuffer vao idx buffer-array 0 stride)
        (loop [attrib-layouts (:attrib-layouts buffer-layout)
               offset 0]
          (let [attrib-layout (first attrib-layouts)
                attrib-count (:count attrib-layout)
                type-glenum (attrib-type->glenum (:type attrib-layout))]
            (GL45/glEnableVertexArrayAttrib vao @attrib-idx)
            (GL45/glVertexArrayAttribBinding vao @attrib-idx idx)
            (condp contains? (:type attrib-layout)
              #{:double}
              (GL45/glVertexArrayAttribLFormat vao @attrib-idx
                                               attrib-count
                                               type-glenum
                                               offset)
              #{:float :half-float}
              (GL45/glVertexArrayAttribFormat vao @attrib-idx
                                              attrib-count
                                              type-glenum
                                              (boolean (:normalized attrib-layout))
                                              offset)
              #{:byte :short :int
                :ubyte :ushort :uint}
              (GL45/glVertexArrayAttribIFormat vao @attrib-idx
                                               attrib-count
                                               type-glenum
                                               offset))
            (vswap! attrib-idx inc)
            (when (seq (rest attrib-layouts))
              (recur (rest attrib-layouts)
                     (long
                      (+ offset (* (attrib-type->size-in-bytes (:type attrib-layout))
                                   (:count attrib-layout)
                                   (if-not (:interleaved buffer-layout)
                                     (:element-count packed-mesh)
                                     1))))))))))
    (.flip buffers)
    (->Mesh vao buffers (when (:indices packed-mesh)
                          (attrib-type->glenum (or (:type (:indices layout))
                                                   :uint)))
            (:element-count packed-mesh)
            (element-type->glenum (:element-type layout))
            (:offset packed-mesh))))
(s/fdef make-mesh
  :args (s/cat :layout ::mesh-layout
               :packed-mesh ::packed-mesh)
  :ret (partial instance? Mesh))

(defn draw-mesh
  "Draws the `mesh` with the current shader pipeline."
  [mesh]
  (let [old-bound (GL45/glGetInteger GL45/GL_VERTEX_ARRAY_BINDING)]
    (GL45/glBindVertexArray (:vao-id mesh))
    (if (:index-type mesh)
      (GL45/glDrawElements (:element-type mesh)
                           (:element-count mesh)
                           (:index-type mesh)
                           (:start-offset mesh))
      (GL45/glDrawArrays (:element-type mesh)
                         (:start-offset mesh)
                         (:element-count mesh)))
    (GL45/glBindVertexArray old-bound))
  nil)
(s/fdef draw-mesh
  :args (s/cat :mesh (partial instance? Mesh))
  :ret nil?)
