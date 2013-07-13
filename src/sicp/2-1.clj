; 2.1 Introduction to Data Abstraction
; ====================================

; Namespace and dependencies

(ns sicp.2-1
  (:require [clojure.math.numeric-tower :as math]))

; math shortcuts

(def abs  math/abs)
(def gcd  math/gcd)
(def sqrt math/sqrt)

(defn square [x] (math/expt x 2))
(defn cube [x] (math/expt x 3))

(defn average [& lst] (/ (reduce + lst) (count lst)))


; Exercise 2.1

(defn make-rat [n d]
  (let [g (gcd n d)
        n (/ n g)
        d (/ d g)]
    (if (< d 0)
      [(- n) (- d)]
      [n d])))
(def numer first)
(def denom second)
; return string as `print` (for REPL)
(defn print-rat [x] (str (numer x) "/" (denom x)))

; try out
(print-rat (make-rat 2 -4))


; Exercise 2.2

(defn make-segment [start end] [start end])
(def start-segment first)
(def end-segment   second)

(defn make-point [x y] [x y])
(def x-point first)
(def y-point second)
(defn print-point [p] (format "(%d, %d)"
                              (x-point p)
                              (y-point p)))

(defn midpoint-segment [seg]
  (let [start (start-segment seg)
        end   (end-segment   seg)]
    (make-point (apply average (map x-point [start end]))
                (apply average (map y-point [start end])))))

; try out
(print-point (midpoint-segment (make-segment (make-point 0 0)
                                             (make-point 2 2))))

; Exercise 2.4

;     (car (cons x y)) -> (car #(% x y)) ->
;     (#(% (fn [p q] p)) #(% x y)) ->
;     (#(#(% x y) (fn [p q] p))) ->
;     ((fn [p q] p) x y) ->
;     ((fn [x y] x)) -> x

(defn cons' [x y] (fn [m] (m x y)))
(defn car' [z] (z (fn [p q] p)))
(defn cdr' [z] (z (fn [p q] q)))

; try out
(car' (cons' :x :y))
(cdr' (cons' :x :y))


; Exercise 2.5

(defn cons' [x y] (* (math/expt 2 x)
                     (math/expt 3 y)))
(defn car' [z]
  (loop [z z, result 0]
    (if (= 0 (mod z 2))
      (recur (/ z 2) (inc result))
      result)))
(defn cdr' [z]
  (loop [z z, result 0]
    (if (= 0 (mod z 3))
      (recur (/ z 3) (inc result))
      result)))

; try out
(car' (cons' 4 5))
(cdr' (cons' 5 6))


; Exercise 2.6

; Church encoding
(def zero (fn [f] (fn [x] x)))
(defn add-1 [n] (fn [f] (fn [x] (f ((n f) x)))))

;     zero  -> λf.λx. x
;     add-1 -> λn.λf.λx. f (n f x)
;
;     add-1 zero -> λf.λx. f (zero f x)
;                -> λf.λx. f ((λf.λx. x) f x)
;                -> λf.λx. f x -> one
;     add-1 one  -> λf.λx. f (one f x)
;                -> λf.λx. f ((λf.λx. f x) f x)
;                -> λf.λx. f (f x) -> two

(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

;     a     -> λf.λx. (f^a) x
;     b     -> λf.λx. (f^b) x
;     a + b -> λf.λx. (f^(a+b)) x
;
;     plus  -> λm.λn.λf.λx. n f (m f x)

(defn church-plus [m n]
  (fn [f] (fn [x] ((n f) ((m f) x)))))


; Exercise 2.7

(defn make-interval [a b] [a b])
(defn upper-bound [[a b]] (max a b))
(defn lower-bound [[a b]] (min a b))


; Exercise 2.8

(defn sub-interval [x y]
  (let [p1 (- (lower-bound x) (lower-bound y))
        p2 (- (lower-bound x) (upper-bound y))
        p3 (- (upper-bound x) (lower-bound y))
        p4 (- (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; try out
(sub-interval (make-interval 1 3)
              (make-interval 0 4))


; Exercise 2.12

(defn make-center-percent [midpoint percent]
  (let [lower (* midpoint (- 1 percent))
        upper (* midpoint (+ 1 percent))]
    (make-interval lower upper)))
(defn percent [i]
  (let [h (upper-bound i)
        l (lower-bound i)]
    (/ (- h l) (+ h l))))

; try out
(percent (make-center-percent 3.5 0.1))
