(ns sicp.utils
  (:refer-clojure :exclude [resolve])
  (:require [clojure.walk :as walk]))

;; Math Functions

(defmacro defmacro-
  "Same as defmacro but yields a private definition"
  [name & decls]
  (list* `defmacro (with-meta name (assoc (meta name) :private true)) decls))

(defmacro- defmathfn-1
  [name]
  (let [java-symbol (symbol "java.lang.Math" (str name))]
    `(do
       (defmulti ~name
         ~(str "Return the " name " of x.")
         {:arglists '([~'x])}
         type)
       (defmethod ~name java.lang.Number
         [~'x]
         (~java-symbol ~'x)))))

(defn- two-types [x y] [(type x) (type y)])

(defmacro- defmathfn-2
  [name]
  (let [java-symbol (symbol "java.lang.Math" (str name))]
    `(do
       (defmulti ~name
         ~(str "Return the " name " of x and y.")
         {:arglists '([~'x ~'y])}
         two-types)
       (defmethod ~name [java.lang.Number java.lang.Number]
         [~'x ~'y]
         (~java-symbol ~'x ~'y)))))

(defmathfn-1 abs)
(defmathfn-1 acos)
(defmathfn-1 asin)
(defmathfn-1 atan)
(defmathfn-2 atan2)
(defmathfn-1 ceil)
(defmathfn-1 cos)
(defmathfn-1 exp)
(defmathfn-1 floor)
(defmathfn-1 log)
(defmathfn-2 pow)
(defmathfn-1 rint)
(defmathfn-1 round)
(defmathfn-1 sin)
(defmathfn-1 sqrt)
(defmathfn-1 tan)

(defn- expt-int [base pow]
  (loop [n pow, y (num 1), z base]
    (let [t (even? n), n (quot n 2)]
      (cond
       t (recur n y (*' z z))
       (zero? n) (*' z y)
       :else (recur n (*' z y) (*' z z))))))

(defn expt
  "(expt base pow) is base to the pow power.
   Returns an exact number if the base is an exact number and the power
   is an integer, otherwise returns a double."
  [base pow]
  (if (and (not (float? base)) (integer? pow))
    (cond
     (pos? pow) (expt-int base pow)
     (zero? pow) 1
     :else (/ 1 (expt-int base (-' pow))))
    (Math/pow base pow)))

(defn gcd
  "(gcd a b) returns the greatest common divisor of a and b"
  [a b]
  (if (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "gcd requires two integers"))
    (loop [a (abs a) b (abs b)]
      (if (zero? b) a,
          (recur b (mod a b))))))

(defn ^boolean divisible?
  [^long dividend ^long divisor]
  (zero? (rem dividend divisor)))

(defn sqr [x] (*' x x))

;; Approximate equality

(defn ^boolean ≈
  ([^double x ^double y] (≈ x y 0.0001))
  ([^double x ^double y ^double tolerance]
     (< (Math/abs (- x y)) tolerance)))

;; Quasiquote

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

;; Letrec

;;; https://gist.github.com/michalmarczyk/486880

(defmacro letrec [bindings & body]
  (let [bcnt (quot (count bindings) 2)
        arrs (gensym "bindings_array")
        arrv `(make-array Object ~bcnt)
        bprs (partition 2 bindings)
        bssl (map first bprs)
        bsss (set bssl)
        bexs (map second bprs)
        arrm (zipmap bssl (range bcnt))
        btes (map #(walk/prewalk (fn [f]
                                   (if (bsss f)
                                     `(aget ~arrs ~(arrm f))
                                     f))
                                 %)
                  bexs)]
    `(let [~arrs ~arrv]
       ~@(map (fn [s e]
                `(aset ~arrs ~(arrm s) ~e))
              bssl
              btes)
       (let [~@(mapcat (fn [s]
                         [s `(aget ~arrs ~(arrm s))])
                       bssl)]
         ~@body))))
