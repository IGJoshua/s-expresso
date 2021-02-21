(ns examples
  "Namespace for running examples from the cli tools."
  (:require
   [examples.window :as window]
   [examples.triangle :as tri]
   [examples.textured-quad :as tex-quad]
   [examples.gravity :as grav]
   [examples.spring :as spring]
   [examples.events :as ev]
   [examples.rendering-system :as rs]))

(defn window
  [args]
  (window/start))
(defn triangle
  [args]
  (tri/start))
(defn textured-quad
  [args]
  (tex-quad/start))
(defn gravity
  [args]
  (grav/start))
(defn spring
  [args]
  (spring/start))
(defn events
  [args]
  (ev/start))
(defn rendering-system
  [args]
  (rs/start))
