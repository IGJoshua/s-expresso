(ns s-expresso.mesh
  (:require
   [clojure.spec.alpha :as s]
   [s-expresso.resource :refer [Resource]])
  (:import
   (org.lwjgl.opengl
    GL45)
   (org.lwjgl.system
    MemoryUtil)))

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
(def attrib-types (set (keys attrib-type->glenum)))
(s/def ::type attrib-types)
(s/def ::count pos-int?)
(s/def ::name keyword?)
(s/def ::convert-fn (s/fspec :args ::vertex
                             :ret (s/coll-of number? :kind vector?)))
(s/def ::normalized boolean?)
(s/def ::attrib-layout (s/keys :req-un [::name ::type ::count]
                               :opt-un [::convert-fn ::normalized]))
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
  [flags]
  (if (zero? (count flags))
    0
    (if (< (count flags) 2)
      (usage-flag->glenum (first flags))
      (apply bit-or (map usage-flag->glenum flags)))))

(s/def ::usage-flag usage-flags)
(s/def ::usage-flags (s/coll-of ::usage-flag :kind set?))
(s/def ::buffer-layout (s/keys :req-un [::attrib-layouts]
                               :opt-un [::interleaved ::usage-flags]))
(s/def ::buffer-layouts (s/coll-of ::buffer-layout :kind vector?))
(s/def ::has-indices boolean?)
(s/def ::mesh-layout (s/keys :req-un [::buffer-layouts ::has-indices]))

(defrecord Mesh [vao-id buffers indexed?]
  Resource
  (free [mesh]
    (GL45/glDeleteBuffers buffers)
    (MemoryUtil/memFree buffers)
    (GL45/glDeleteVertexArrays vao-id)))
