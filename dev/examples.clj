(ns examples
  "Namespace for running examples from the cli tools."
  (:require
   [examples.window :as window]
   [examples.triangle :as tri]
   [examples.textured-quad :as tex-quad]))

(defn window
  [args]
  (window/start))
(defn triangle
  [args]
  (tri/start))
(defn textured-quad
  [args]
  (tex-quad/start))
