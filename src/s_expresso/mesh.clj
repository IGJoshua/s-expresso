(ns s-expresso.mesh
  (:require
   [s-expresso.resource :refer [Resource]])
  (:import
   (org.lwjgl.opengl
    GL45)
   (org.lwjgl.system
    MemoryUtil)))

(defrecord Mesh [vao-id buffers indexed?]
  Resource
  (free [mesh]
    (GL45/glDeleteBuffers buffers)
    (MemoryUtil/memFree buffers)
    (GL45/glDeleteVertexArrays vao-id)))
