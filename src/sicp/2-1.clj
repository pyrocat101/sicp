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


; Exercise 2.17

(defn last-pair [items]
  (let [rest (rest items)]
    (if (empty? rest)
      items
      (last-pair rest))))

(last-pair (list 23 72 149 34))


; Exercise 2.18

(defn reverse' [items]
  (loop [reversed nil remain items]
    (if (empty? remain)
      reversed
      (recur (cons (first remain) reversed)
             (rest remain)))))


(reverse' '())
(reverse' '(1 4 9 16 25))


; Exercise 2.19

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(def no-more? empty?)
(def except-first-denomination rest)
(def first-denomination first)

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values))))

; test drive
(cc 100 us-coins)
;; (cc 100 uk-coins)
; order of values does not matter
(cc 100 (reverse us-coins))
;; (cc 100 (reverse uk-coins))


; Exercise 2.20

(defn same-parity [& items]
  (if (odd? (first items))
    (filter odd? items)
    (filter even? items)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


; Exercise 2.21

(defn square-list-1 [items]
  (if (empty? items)
    nil
    (cons (square (first items))
          (square-list-1 (rest items)))))

(defn square-list-2 [items] (map square items))

(square-list-1 '(1 2 3 4))
(square-list-2 '(1 2 3 4))


; Exercise 2.22

(defn iterative-square-list [items]
  (loop [things (reverse items) answer nil]
    (if (empty? things)
      answer
      (recur (rest things)
             (cons (square (first things))
                   answer)))))

(iterative-square-list '(1 2 3 4))


; Exercise 2.23

(defn for-each [f items]
  (if-not (empty? items)
    (do
      (f (first items))
      (recur f (rest items)))))

;; (for-each println '(57 321 88))


; Exercise 2.25

(def car first)
(def cdr rest)
(def cadr #(car (cdr %)))
(def cdar #(cdr (car %)))
(def caar #(car (car %)))
(def cddr #(cdr (cdr %)))

(cdar (cddr '(1 3 (5 7) 9)))
(caar '((7)))
(nth (iterate cadr '(1 (2 (3 (4 (5 (6 7))))))) 6)


; Exercise 2.27

(defn deep-reverse [items]
  (loop [reversed nil remain items]
    (if (empty? remain)
      reversed
      (let [head
            (if (seq? (first remain))
              (deep-reverse (first remain))
              (first remain))]
        (recur (cons head reversed)
               (rest remain))))))

(deep-reverse '((1 2) (3 4)))


; Exercise 2.28

(defn fringe [tree]
  (cond (= tree '()) []
        (not (seq? tree)) [tree]
        :else (concat (fringe (first tree))
                      (fringe (rest  tree)))))

(fringe '((1 2) (3 4)))


; Exercise 2.30

(defn square-tree-1 [tree]
  (cond (= tree '()) '()
        (not (seq? tree)) (square tree)
        :else (cons (square-tree-1 (first tree))
                    (square-tree-1 (rest  tree)))))
(square-tree-1 '(1 (2 (3 4) 5 (6 7))))

(defn square-tree-2 [tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (square-tree-2 sub-tree)
           (square sub-tree)))
       tree))
(square-tree-2 '(1 (2 (3 4) 5 (6 7))))


; Exercise 2.31

(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(defn square-tree-3 [tree] (tree-map square tree))
(square-tree-3 '(1 (2 (3 4) 5 (6 7))))


; Exercise 2.32

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1)
          (append (rest list1) list2))))

(defn subsets [s]
  (if (= s '())
    '(())
    (let [rest (subsets (rest s))]
      (append (map #(cons (first s) %) rest) rest))))
(subsets '(1 2 3))


; Exercise 2.33

(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence)
        (accumulate op initial (rest sequence)))))

(defn map' [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) '() sequence))
(defn append' [seq1 seq2]
  (accumulate cons seq2 seq1))
(defn length' [sequence]
  (accumulate (fn [x y] (inc y)) 0 sequence))

(map' square '(1 2 3 4 5))
(append' '(1 2) '(3 4))
(length' '(1 2 3 4 5))


; Exercise 2.34

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


; Exercise 2.35

(defn count-leaves [t]
  (accumulate +
              0
              (map (fn [tree]
                     (if (seq? tree)
                       (count-leaves tree)
                       1))
                   t)))
(count-leaves '((1 2) (3 4)))


; Exercise 2.36

(defn accumulate-n [op init seqs]
  (if (= (first seqs) '())
    '()
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))
(accumulate-n + 0 '((1  2  3)
                    (4  5  6)
                    (7  8  9)
                    (10 11 12)))


; Exercise 2.37

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product % v) m))

(defn transpose [mat]
  (accumulate-n cons '() mat))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

(def m '((1 2 3 4)
         (4 5 6 6)
         (6 7 8 9)))

(matrix-*-vector m '(1 2 3 4))
(transpose m)
(matrix-*-matrix m (transpose m))


; Exercise 2.38

; `op`'s operands must be commutative


