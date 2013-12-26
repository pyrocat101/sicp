(ns sicp.test_helper)

(defn ^boolean ≈
  ([^double x ^double y] (≈ x y 0.0001))
  ([^double x ^double y ^double tolerance]
     (< (Math/abs (- x y)) tolerance)))

(defn unquote? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote)))

(defn unquote-splicing? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote-splicing)))

(defn record? [x]
  (instance? clojure.lang.IRecord x))

(defn- template-fn [form]
  (cond
   (symbol? form) `'~form
   (unquote? form) (second form)
   (unquote-splicing? form) (throw (Exception. "splice not in list"))
   (record? form) `'~form
   (coll? form)
   (let [xs (if (map? form) (apply concat form) form)
         parts (for [x (partition-by unquote-splicing? xs)]
                 (if (unquote-splicing? (first x))
                   (second (first x))
                   (mapv template-fn x)))
         cat (doall `(concat ~@parts))]
     (cond
      (vector? form) `(vec ~cat)
      (map? form) `(apply hash-map ~cat)
      (set? form) `(set ~cat)
      (seq? form) `(apply list ~cat)
      :else (throw (Exception. "Unknown collection type"))))
   :else `'~form))

(defmacro template [form]
  (template-fn form))
