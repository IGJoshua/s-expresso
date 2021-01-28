(ns s-expresso.math
  "Utilities for game math.")

(defn log
  "Calculates the logarithm of the value at the given base."
  ([n] (Math/log n))
  ([n base]
   (/ (Math/log n)
      (Math/log base))))
