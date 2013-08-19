(ns sicp.test.helper)

(defn ^boolean ≈
  ([^double x ^double y] (≈ x y 0.0001))
  ([^double x ^double y ^double tolerance]
   (< (Math/abs (- x y)) tolerance)))
