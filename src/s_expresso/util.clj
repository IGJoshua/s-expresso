(ns s-expresso.util
  "Basic and general utilities.")

(defn if-pred
  "If calling `pred` on `v` returns truthy, call `then` on `v`, otherwise call
  `else` on it.

  See also [[when-pred]]."
  [v pred then else]
  (if (pred v) (then v) (else v)))

(defn when-pred
  "If calling `pred` on `v` returns truthy, call `then` on `v`, otherwise return
  it unmodified."
  [v pred then]
  (if (pred v) (then v) v))
