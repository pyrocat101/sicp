;;;; 2.2 Hierarchical Data and the Closure Property
;;;; ==============================================

;;; Namespace and dependencies

(ns sicp.2-2
  (:require [clojure.math.numeric-tower :as math]))

;;; Math shortcuts

(def abs  math/abs)
(def gcd  math/gcd)
(def sqrt math/sqrt)

(defn square [x] (math/expt x 2))
(defn cube [x] (math/expt x 3))


;;; Exercise 2.17

(defn last-pair [items]
  (let [rest (rest items)]
    (if (empty? rest)
      items
      (last-pair rest))))

(last-pair (list 23 72 149 34))


;;; Exercise 2.18

(defn reverse' [items]
  (loop [reversed nil remain items]
    (if (empty? remain)
      reversed
      (recur (cons (first remain) reversed)
             (rest remain)))))


(reverse' '())
(reverse' '(1 4 9 16 25))


;;; Exercise 2.19

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

;; test drive
(cc 100 us-coins)
;; (cc 100 uk-coins)
;; order of values does not matter
(cc 100 (reverse us-coins))
;; (cc 100 (reverse uk-coins))


;;; Exercise 2.20

(defn same-parity [& items]
  (if (odd? (first items))
    (filter odd? items)
    (filter even? items)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


;;; Exercise 2.21

(defn square-list-1 [items]
  (if (empty? items)
    nil
    (cons (square (first items))
          (square-list-1 (rest items)))))

(defn square-list-2 [items] (map square items))

(square-list-1 '(1 2 3 4))
(square-list-2 '(1 2 3 4))


;;; Exercise 2.22

(defn iterative-square-list [items]
  (loop [things (reverse items) answer nil]
    (if (empty? things)
      answer
      (recur (rest things)
             (cons (square (first things))
                   answer)))))

(iterative-square-list '(1 2 3 4))


;;; Exercise 2.23

(defn for-each [f items]
  (if-not (empty? items)
    (do
      (f (first items))
      (recur f (rest items)))))

;; (for-each println '(57 321 88))


;;; Exercise 2.25

(def car first)
(def cdr rest)
(def cadr #(car (cdr %)))
(def cdar #(cdr (car %)))
(def caar #(car (car %)))
(def cddr #(cdr (cdr %)))

(cdar (cddr '(1 3 (5 7) 9)))
(caar '((7)))
(nth (iterate cadr '(1 (2 (3 (4 (5 (6 7))))))) 6)


;;; Exercise 2.27

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


;;; Exercise 2.28

(defn fringe [tree]
  (cond (= tree '()) []
        (not (seq? tree)) [tree]
        :else (concat (fringe (first tree))
                      (fringe (rest  tree)))))

(fringe '((1 2) (3 4)))


;;; Exercise 2.30

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


;;; Exercise 2.31

(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(defn square-tree-3 [tree] (tree-map square tree))
(square-tree-3 '(1 (2 (3 4) 5 (6 7))))


;;; Exercise 2.32

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


;;; Exercise 2.33

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


;;; Exercise 2.34

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


;;; Exercise 2.35

(defn count-leaves [t]
  (accumulate +
              0
              (map (fn [tree]
                     (if (seq? tree)
                       (count-leaves tree)
                       1))
                   t)))
(count-leaves '((1 2) (3 4)))


;;; Exercise 2.36

(defn accumulate-n [op init seqs]
  (if (= (first seqs) '())
    '()
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))
(accumulate-n + 0 '((1  2  3)
                    (4  5  6)
                    (7  8  9)
                    (10 11 12)))


;;; Exercise 2.37

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


;;; Exercise 2.38

;;; `op` must be commutative


;;; Exercise 2.39

(def fold-right accumulate)
(defn fold-left [op initial sequence]
  (loop [result initial, remain sequence]
    (if (= remain '())
      result
      (recur (op result (first remain))
             (rest remain)))))

(defn reverse-fold-right [sequence]
  (fold-right (fn [x y] (append y (list x))) '() sequence))
(reverse-fold-right '(1 2 3 4 5))

(defn reverse-fold-left [sequence]
  (fold-left (fn [x y] (cons y x)) '() sequence))
(reverse-fold-left '(1 2 3 4 5))


;;; Exercise 2.40

(defn enumerate-interval [low high]
  (range low (inc high)))

(defn flatmap [proc seq]
  (accumulate append '() (map proc seq)))

(defn unique-pairs [n]
  (flatmap (fn [i]
             (map (fn [j] (list i j))
                  (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(defn divisible? [n d] (zero? (rem n d)))

(defn prime? [n]
  (cond (or (= n 2) (= n 3)) true
        (or (divisible? n 2) (< n 2)) false
        :else (let [sqrt-n (Math/sqrt n)]
                (loop [i 3]
                  (cond (divisible? n i) false
                        (< sqrt-n i) true
                        :else (recur (+ i 2)))))))

(defn prime-sum? [pair]
  (prime? (+ (car pair) (cadr pair))))

(defn make-pair-sum [pair]
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)


;;; Exercise 2.41

(defn unique-triples [n]
  (flatmap (fn [i]
             (map (fn [j] (cons i j))
                  (unique-pairs (dec i))))
           (enumerate-interval 1 n)))

(defn unique-triples-sum-equal-to [n]
  (filter #(= n (reduce + %)) (unique-triples n)))

(unique-triples-sum-equal-to 10)


;;; Exercise 2.42

(def empty-board '())

(defn safe? [k positions]
  (let [row-of-new-queen (first positions)]
    (loop [rest-of-queens (rest positions), i 1]
      (let [row-of-current-queen (first rest-of-queens)]
        (cond (= '() rest-of-queens) true
              (or (= row-of-new-queen
                     row-of-current-queen)
                  (= (- row-of-new-queen i)
                     row-of-current-queen)
                  (= (+ row-of-new-queen i)
                     row-of-current-queen)) false
              :else (recur (rest rest-of-queens) (inc i)))))))

(defn adjoin-position [new-row k rest-of-queens]
  (cons new-row rest-of-queens))

(defn queens [board-size]
  (letfn [(queen-cols [k]
    (if (= k 0)
      (list empty-board)
      (filter
       (fn [positions] (safe? k positions))
       (flatmap
        (fn [rest-of-queens]
          (map (fn [new-row]
                 (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols (dec k))))))]
    (queen-cols board-size)))

(queens 8)
