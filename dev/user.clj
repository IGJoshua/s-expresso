(ns user
  "REPL utilities namespace.
  Contains utility functions and other repl nicities for working with OpenGL,
  GLFW, and other parts of LWJGL at the repl, and for interacting with the other
  namespaces.

  This namespace should only be loaded during development, and not during
  production."
  (:require
   [clojure.java.javadoc :as jd]))

(defn add-javadocs
  "Adds the relevant javadoc sites for libraries which I will be using."
  []
  (jd/add-remote-javadoc "org.lwjgl" "https://javadoc.lwjgl.org/"))

(defn on-load
  "Runs any user code which should be run at startup when developing."
  []
  (add-javadocs)
  true)

(defonce
  ^{:doc
    "A throw-away value returned from on-load.

    This value exists in a defonce so that `(on-load)` will be run once at load
    time for the `user` namespace."}
  on-load-hook (on-load))
