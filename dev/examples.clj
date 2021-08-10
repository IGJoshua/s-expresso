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
  [_]
  (window/start))
(defn triangle
  [_]
  (tri/start))
(defn textured-quad
  [_]
  (tex-quad/start))
(defn gravity
  [_]
  (grav/start))
(defn spring
  [_]
  (spring/start))
(defn events
  [_]
  (ev/start))
(defn rendering-system
  [_]
  (rs/start))
