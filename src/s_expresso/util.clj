(ns s-expresso.util
  "Basic and general utilities.")

(defn if-pred
  "If calling `pred` on `v` returns truthy, call `then` on `v`, otherwise call `else` on it.

  See [[when-pred]]."
  ([pred then else]
   (fn [v]
     (if-pred v pred then else)))
  ([v pred then else]
   (if (pred v) (then v) (else v))))

(defn when-pred
  "If calling `pred` on `v` returns truthy, call `then` on `v`, otherwise return it unmodified.

  See [[if-pred]]."
  ([pred then]
   (fn [v]
     (when-pred v pred then)))
  ([v pred then]
   (if (pred v) (then v) v)))
