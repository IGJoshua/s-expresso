(ns s-expresso.memory
  (:import
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
  memory utils are unaffected, however any code which allocates buffers by usage
  of the code here will allocate to the stack. Any memory allocated on the stack
  which escapes the scope and is used is a use-after-free bug."
  [& body]
  `(with-open [stack# (MemoryStack/stackPush)]
     (binding [*memory-stack* stack#]
       ~@body)))

(defprotocol IntoByteBuffer
  (put [v buf])
  (put-at [v buf byte-offset]))

(extend-protocol IntoByteBuffer
  Float
  (put [v buf]
    (.putFloat buf v))
  (put-at [v buf byte-offset]
    (.putFloat buf (/ byte-offset Float/BYTES) v))

  Double
  (put [v buf]
    (.putDouble buf v))
  (put-at [v buf byte-offset]
    (.putDouble buf (/ byte-offset Double/BYTES) v))

  Byte
  (put [v buf]
    (.putByte buf v))
  (put-at [v buf byte-offset]
    (.putByte buf byte-offset v))

  Short
  (put [v buf]
    (.putShort buf v))
  (put-at [v buf byte-offset]
    (.putShort buf (/ byte-offset Short/BYTES) v))

  Integer
  (put [v buf]
    (.putInt buf v))
  (put-at [v buf byte-offset]
    (.putInt buf (/ byte-offset Integer/BYTES) v)))

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
  [num-bytes]
  (if *memory-stack*
    (.calloc *memory-stack* num-bytes)
    (BufferUtils/createByteBuffer num-bytes)))
