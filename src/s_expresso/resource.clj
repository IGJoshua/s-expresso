(ns s-expresso.resource
  "Defines a protocol for unmanaged system resources.

  Such resources must be handled manually, like GPU memory, OS windows, etc.,
  and require special code to be released."
  (:import
   (java.io Closeable)))

(defprotocol Resource
  "Resources handle the process of freeing unmanaged resources."
  :extend-via-metadata true
  (free [live-resource] "Frees a live resource"))

(extend-protocol Resource
  nil
  (free [_] nil)

  Object
  (free [_] nil)

  Closeable
  (free [res] (.close res)))

(defmacro with-free
  [bindings & body]
  (assert (even? (count bindings)))
  (if (seq bindings)
    `(let ~(vec (take 2 bindings))
       (try
         (with-free ~(vec (drop 2 bindings)) ~@body)
         (finally
           (free ~(first bindings)))))
    (cons `do body)))
