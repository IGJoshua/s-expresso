(ns s-expresso.math
  "Utilities for game math.")

(defn log
  "Calculates the logarithm of the value at the given base."
  [n base]
  (/ (Math/log n)
     (Math/log base)))
