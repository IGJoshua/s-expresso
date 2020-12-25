(ns s-expresso.texture
  "Functions for loading images and uploading them to the GPU."
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
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
    (STBImage/stbi_image_free data)))

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

(defn upload-image
  "Uploads an `image` to the GPU."
  [image]
  (let [tex-id (GL45/glCreateTextures GL45/GL_TEXTURE_2D)]
    ))
(s/fdef upload-image
  :args (s/cat :image ::image)
  :ret nil?)
