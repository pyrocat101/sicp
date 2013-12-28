(ns sicp.utils)

(defn ^boolean ≈
  ([^double x ^double y] (≈ x y 0.0001))
  ([^double x ^double y ^double tolerance]
     (< (Math/abs (- x y)) tolerance)))

(def ^:dynamic ^:private *gensyms*)

(defn- resolve [sym]
  (let [ns (namespace sym)
        n (name sym)]
    (if (and (not ns) (= (last n) \#))
      (if-let [gs (@*gensyms* sym)]
        gs
        (let [gs (gensym (str (subs n 0 (dec (count n))) "__auto__"))]
          (swap! *gensyms* assoc sym gs)
          gs))
      sym)))

(defn unquote? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote)))

(defn unquote-splicing? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote-splicing)))

(defn record? [x]
  (instance? clojure.lang.IRecord x))

(defn- quasiquote-fn* [form]
  (cond
   (symbol? form) `'~(resolve form)
   (unquote? form) (second form)
   (unquote-splicing? form) (throw (Exception. "splice not in list"))
   (record? form) `'~form
   (coll? form)
   (let [xs (if (map? form) (apply concat form) form)
         parts (for [x (partition-by unquote-splicing? xs)]
                 (if (unquote-splicing? (first x))
                   (second (first x))
                   (mapv quasiquote-fn* x)))
         cat (doall `(concat ~@parts))]
     (cond
      (vector? form) `(vec ~cat)
      (map? form) `(apply hash-map ~cat)
      (set? form) `(set ~cat)
      (seq? form) `(apply list ~cat)
      :else (throw (Exception. "Unknown collection type"))))
   :else `'~form))

(defn quasiquote-fn [form]
  (binding [*gensyms* (atom {})]
    (quasiquote-fn* form)))

(defmacro quasiquote [form]
  (quasiquote-fn form))
