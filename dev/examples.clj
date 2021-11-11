(ns examples
  "Namespace for running examples from the cli tools."
  (:require
   [examples.window :as window]
   [examples.triangle :as tri]
   [examples.textured-quad :as tex-quad]
   [examples.gravity :as grav]
   [examples.spring :as spring]
   [examples.events :as ev]
   [examples.audio :as audio]))

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
(defn audio
  [_]
  (audio/start))
