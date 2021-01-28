(ns s-expresso.memory
  "Namespace with helper functions for allocating memory and stack allocation."
  (:import
   (java.nio
    ByteBuffer)
   (mikera arrayz.INDArray
           vectorz.AVector)
   (org.lwjgl
    BufferUtils)
   (org.lwjgl.opengl
    GL45)
   (org.lwjgl.system
    MemoryStack
    MemoryUtil)))

(def ^{:private true
       :dynamic true}
  *memory-stack*
  "Holds a memory stack that buffers can be allocated from.
  Any stack-allocated memory which escapes from the scope of the allocator is
  freed, and any usage of it becomes a use-after-free bug."
  nil)

(defmacro with-stack-allocator
  "Creates a stack allocator within the dynamic scope of this macro body.
  This should be used carefully. Places which directly interact with lwjgl's
  memory utils are unaffected. Any memory allocated on the stack which escapes
  the scope and is used is a use-after-free bug."
  [& body]
  `(with-open [stack# (MemoryStack/stackPush)]
     (binding [*memory-stack* stack#]
       ~@body)))

(defmacro with-heap-allocator
  "Ensures allocations within the dynamic scope of this macro body are on the heap.
  The primary usage of this is for functions which wish to provide an interface
  compatible with usage of [[s-expresso.memory/with-stack-allocator]] but which
  needs to create a long-lived allocation. Places which directly interact with
  lwjgl's memory utils are unaffected."
  [& body]
  `(binding [*memory-stack* nil]
    ~@body))

(defprotocol IntoByteBuffer
  "Provides a way to serialize a type into a [[java.nio.ByteBuffer]]."
  :extend-via-metadata true
  (put [v buf] "Puts the value into the byte buffer at the cursor.")
  (put-at [v buf byte-offset] "Puts the value into the byte buffor at the given offset."))

(extend-protocol IntoByteBuffer
  Float
  (put [v buf]
    (.putFloat ^ByteBuffer buf v))
  (put-at [v buf byte-offset]
    (.putFloat ^ByteBuffer buf byte-offset v))

  Double
  (put [v buf]
    (.putDouble ^ByteBuffer buf v))
  (put-at [v buf byte-offset]
    (.putDouble ^ByteBuffer buf byte-offset v))

  Byte
  (put [v buf]
    (.put ^ByteBuffer buf v))
  (put-at [v buf byte-offset]
    (.put ^ByteBuffer buf byte-offset v))

  Short
  (put [v buf]
    (.putShort ^ByteBuffer buf v))
  (put-at [v buf byte-offset]
    (.putShort ^ByteBuffer buf byte-offset v))

  Integer
  (put [v buf]
    (.putInt ^ByteBuffer buf v))
  (put-at [v buf byte-offset]
    (.putInt ^ByteBuffer buf byte-offset v))

  AVector
  (put [v ^ByteBuffer buf]
    (put-seq buf (seq (.asElementList v))))
  (put-at [v ^ByteBuffer buf byte-offset]
    (loop [elements (seq (.asElementList v))
           idx 0]
      (when (seq elements)
        (.putDouble buf (+ byte-offset
                           (* idx Double/BYTES))
                    (first elements))
        (recur (rest elements) (inc idx)))))

  INDArray
  (put [v ^ByteBuffer buf]
    (put-seq buf (seq (.asElementList v))))
  (put-at [v ^ByteBuffer buf byte-offset]
    (loop [elements (seq (.asElementList v))
           idx 0]
      (when (seq elements)
        (.putDouble buf (+ byte-offset
                           (* idx Double/BYTES))
                    (first elements))
        (recur (rest elements) (inc idx))))))

(defn put-seq
  "Puts each item from a sequence onto a memory buffer.
  This will put each element onto the buffer in sequence, using the appropriate
  put operation based on the item's type, meaning the sequence can be
  heterogeneous.
  Returns the buffer."
  [buf s]
  (run! #(put % buf) (seq s))
  buf)

(defn alloc-bytes
  "Allocates a number of bytes as a [[java.nio.ByteBuffer]].
  This allocation is done using [[org.lwjgl.system.MemoryUtil]] by default, but
  if this is called within the dynamic extent of a [[with-stack-allocator]] use,
  it will allocate the buffer on the stack, and it will be invalid outside of
  that scope."
  ^ByteBuffer [num-bytes]
  (if *memory-stack*
    (.calloc ^MemoryStack *memory-stack* num-bytes)
    (BufferUtils/createByteBuffer num-bytes)))
