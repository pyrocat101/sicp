; 1.3 Formulating Abstractions with Higher-Order Procedures
; =========================================================

; Namespace and dependencies

(ns sicp.1-3
  (:require [clojure.math.numeric-tower :as math]))

; Newton Integral

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn newton-integral [f a b dx]
  (let [add-dx #(+ % dx)]
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

(defn square [x] (math/expt x 2))
(defn cube [x] (math/expt x 3))

(newton-integral cube 0 1 0.01)
(newton-integral cube 0 1 0.001)

; Exercise 1.29

(defn simpson-integral [f a b n]
  (let [h    (/ (- b a) n)
        y    (fn [k] (f (+ a (* k h))))
        next (fn [k] (+ k 2))
        term (fn [n] (+ (y (dec n))
                        (* 4 (y n))
                        (y (inc n))))]
    (* (/ h 3) (sum term 1 next (dec n)))))

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

; Exercise 1.30

(defn iterative-sum [term a next b]
  (loop [a a, result 0]
    (if (> a b)
      result
      (recur (next a)
             (+ (term a) result)))))

; Exercise 1.31

; a)

(defn product [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(defn factorial [n]
  (product identity 1 inc n))

(factorial 10)

(defn john-wallis [n]
  (product #(/ (* (inc %) (dec %))
               (square %))
           3.0
           #(+ % 2)
           (+ 1 (* n 2))))

(* 4 (john-wallis 100))

; b)

(defn iterative-product [term a next b]
  (loop [a a, result 1]
    (if (> a b)
      result
      (recur (next a)
             (* (term a) result)))))

; Exercise 1.32

(defn accumulate
  [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term a next b))))

(defn accumulative-sum
  [term a next b]
  (accumulate + 0 term a next b))

(defn accumulative-product
  [term a next b]
  (accumulate * 1 term a next b))

; Exercise 1.33




