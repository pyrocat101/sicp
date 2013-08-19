;;;; Chapter 2: Building Abstractions with Data
;;;; ==========================================

;;; Namespace and dependencies

(ns sicp.ch2
  (:require [clojure.math.numeric-tower :refer (gcd sqrt expt)]
            [clojure.contrib.generic.math-functions :refer (sin cos atan2)]))

(defn square [x] (* x x))
(defn cube [x] (* x x x))
(defn average [& lst] (/ (reduce + lst) (count lst)))

(def car first)
(def cdr rest)
(def caar (comp car car))
(def cddr (comp cdr cdr))
(def cadr (comp car cdr))
(def cdar (comp cdr car))
(def caddr (comp car cddr))
(def cadar (comp car cdar))
(def cdddr (comp cdr cddr))
(def null? (comp not seq))

;;; Exercise 2.1

(defn make-rat [n d]
  (let [g (gcd n d)
        n (/ n g)
        d (/ d g)]
    (if (< d 0)
      [(- n) (- d)]
      [n d])))

(def numer first)
(def denom second)

(defn print-rat [x] (str (numer x) "/" (denom x)))

;;; Exercise 2.2

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

(print-point (midpoint-segment (make-segment (make-point 0 0)
                                             (make-point 2 2))))

;;; Exercise 2.4

;;;   (car (cons x y)) -> (car #(% x y)) ->
;;;   (#(% (fn [p q] p)) #(% x y)) ->
;;;   (#(#(% x y) (fn [p q] p))) ->
;;;   ((fn [p q] p) x y) ->
;;;   ((fn [x y] x)) -> x

(defn cons' [x y] (fn [m] (m x y)))
(defn car' [z] (z (fn [p q] p)))
(defn cdr' [z] (z (fn [p q] q)))

;;; Exercise 2.5

(defn cons'' [x y] (* (expt 2 x)
                      (expt 3 y)))
(defn car'' [z]
  (loop [z z
         result 0]
    (if (= 0 (mod z 2))
      (recur (/ z 2) (inc result))
      result)))

(defn cdr'' [z]
  (loop [z z
         result 0]
    (if (= 0 (mod z 3))
      (recur (/ z 3) (inc result))
      result)))

;;; Exercise 2.6

;; Church encoding
(def zero (fn [f] (fn [x] x)))
(defn add-1 [n] (fn [f] (fn [x] (f ((n f) x)))))

;;;     zero  -> λf.λx. x
;;;     add-1 -> λn.λf.λx. f (n f x)
;;;
;;;     add-1 zero -> λf.λx. f (zero f x)
;;;                -> λf.λx. f ((λf.λx. x) f x)
;;;                -> λf.λx. f x -> one
;;;     add-1 one  -> λf.λx. f (one f x)
;;;                -> λf.λx. f ((λf.λx. f x) f x)
;;;                -> λf.λx. f (f x) -> two

(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

;;;     a     -> λf.λx. (f^a) x
;;;     b     -> λf.λx. (f^b) x
;;;     a + b -> λf.λx. (f^(a+b)) x
;;;
;;;     plus  -> λm.λn.λf.λx. n f (m f x)

(defn church-plus [m n]
  (fn [f] (fn [x] ((n f) ((m f) x)))))

;;; Exercise 2.7

(defn make-interval [a b] [a b])
(defn upper-bound [[a b]] (max a b))
(defn lower-bound [[a b]] (min a b))

;;; Exercise 2.8

(defn sub-interval [x y]
  (let [p1 (- (lower-bound x) (lower-bound y))
        p2 (- (lower-bound x) (upper-bound y))
        p3 (- (upper-bound x) (lower-bound y))
        p4 (- (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;;; Exercise 2.12

(defn make-center-percent [midpoint percent]
  (let [lower (* midpoint (- 1 percent))
        upper (* midpoint (+ 1 percent))]
    (make-interval lower upper)))
(defn percent [i]
  (let [h (upper-bound i)
        l (lower-bound i)]
    (/ (- h l) (+ h l))))

;;; Exercise 2.17

(defn last-pair [items]
  (let [rest (rest items)]
    (if (empty? rest)
      items
      (last-pair rest))))

;;; Exercise 2.18

(defn reverse' [items]
  (loop [reversed '() remain items]
    (if (empty? remain)
      reversed
      (recur (cons (first remain) reversed)
             (rest remain)))))

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

;; order of values does not matter

;;; Exercise 2.20

(defn same-parity [& items]
  (if (odd? (first items))
    (filter odd? items)
    (filter even? items)))

;;; Exercise 2.21

(defn square-list-1 [items]
  (if (empty? items)
    nil
    (cons (square (first items))
          (square-list-1 (rest items)))))

(defn square-list-2 [items] (map square items))

;;; Exercise 2.22

(defn iterative-square-list [items]
  (loop [things (reverse items) answer nil]
    (if (empty? things)
      answer
      (recur (rest things)
             (cons (square (first things))
                   answer)))))

;;; Exercise 2.23

(defn for-each [f items]
  (if-not (empty? items)
    (do
      (f (first items))
      (recur f (rest items)))))

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

;;; Exercise 2.28

(defn fringe [tree]
  (cond (= tree '()) []
        (not (seq? tree)) [tree]
        :else (concat (fringe (first tree))
                      (fringe (rest  tree)))))

;;; Exercise 2.30

(defn square-tree-1 [tree]
  (cond (= tree '()) '()
        (not (seq? tree)) (square tree)
        :else (cons (square-tree-1 (first tree))
                    (square-tree-1 (rest  tree)))))

(defn square-tree-2 [tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (square-tree-2 sub-tree)
           (square sub-tree)))
       tree))

;;; Exercise 2.31

(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(defn square-tree-3 [tree] (tree-map square tree))

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

;;; Exercise 2.34

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;; Exercise 2.35

(defn count-leaves [t]
  (accumulate +
              0
              (map (fn [tree]
                     (if (seq? tree)
                       (count-leaves tree)
                       1))
                   t)))

;;; Exercise 2.36

(defn accumulate-n [op init seqs]
  (if (= (first seqs) '())
    '()
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

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

;;; Exercise 2.38

;;; `op` must be commutative

;;; Exercise 2.39

(def fold-right accumulate)

(defn fold-left [op initial sequence]
  (loop [result initial
         remain sequence]
    (if (= remain '())
      result
      (recur (op result (first remain))
             (rest remain)))))

(defn reverse-fold-right [sequence]
  (fold-right (fn [x y] (append y (list x))) '() sequence))

(defn reverse-fold-left [sequence]
  (fold-left (fn [x y] (cons y x)) '() sequence))

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
        :else (let [sqrt-n (sqrt n)]
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

;;; Exercise 2.41

(defn unique-triples [n]
  (flatmap (fn [i]
             (map (fn [j] (cons i j))
                  (unique-pairs (dec i))))
           (enumerate-interval 1 n)))

(defn unique-triples-sum-equal-to [n]
  (filter #(= n (reduce + %)) (unique-triples n)))

;;; Exercise 2.42

(def empty-board '())

(defn safe? [k positions]
  (let [row-of-new-queen (first positions)]
    (loop [rest-of-queens (rest positions)
           i 1]
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

;;; Exercise 2.54

(defn equal? [list1 list2]
  (cond (or (= list1 '()) (= list2 '()))   true
        (not= (first list1) (first list2)) false
        :else (recur (rest list1) (rest list2))))

;;; Exercise 2.55

;;; (car ''abracadabra)
;;; -> (car (quote (quote abracadabra)))
;;; -> quote

;;; Exercise 2.56

(def variable? symbol?)

(defn same-variable? [& vars]
  (and (every? variable? vars)
       (apply = vars)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn sum? [x] (and (seq? x) (= (car x) '+)))
(def addend cadr)

(defn product? [x] (and (seq? x) (= (car x) '*)))
(def multiplier   cadr)

(declare augend multiplicand)

(defn exponent? [x] (and (seq? x) (= (car x) '**)))
(def base cadr)
(def exponent caddr)

(defn make-exponentiation [base exp]
  (cond (and (number? base)
             (number? exp)) (expt base exp)
        (=number? exp  0)   1
        (=number? exp  1)   base
        :else (list '** base exp)))

(defmulti deriv
  (fn [exp var]
    (cond (number?   exp) :number
          (variable? exp) :variable
          (sum?      exp) :sum
          (product?  exp) :product
          (exponent? exp) :exponent
          :else           :unknown)))

(defmethod deriv :number [exp var] 0)

(defmethod deriv :variable
  [exp var]
  (if (same-variable? exp var) 1 0))

(defmethod deriv :sum
  [exp var]
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(defmethod deriv :product
  [exp var]
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))

(defmethod deriv :exponent
  [exp var]
  (let [base (base     exp)
        exp  (exponent exp)]
    (make-product
     exp
     (make-product (make-exponentiation base (dec exp))
                   (deriv base var)))))

(defmethod deriv :unknown
  [exp var]
  (print-str "unknown expression type -- DERIV" exp))

;;; Exercise 2.57

(defn augend [s]
  (if-not (seq (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(defn multiplicand [p]
  (if-not (seq (cdddr p))
    (caddr p)
    (cons '* (cddr p))))

;;; Exercise 2.59

(defn element-of-set? [x set]
  (cond (null? set) false
        (= x (car set)) true
        :else (recur x (cdr set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond (or (null? set1) (null? set2)) '()
        ;;
        (element-of-set? (car set1) set2)
        (cons (car set1) (intersection-set (cdr set1) set2))
        ;;
        :else (recur (cdr set1) set2)))

(defn union-set [set1 set2]
  (cond (or (null? set1) (null? set2)) '()
        (element-of-set? (car set1) set2) (recur (cdr set1) set2)
        :else (cons (car set1) (union-set (cdr set1) set2))))

;;; Exercise 2.60

;; most functions remains unmodified

(def elements-of-dup-set? element-of-set?)
(def adjoin-dup-set cons)
(def union-dup-set concat)
(def intersection-dup-set intersection-set)

;;; For scenarios that frequently adjoins new elements into the set
;;; or unions two sets, this representation is more efficient.

;;; Exercise 2.61

(defn elements-of-sorted-set? [x set]
  (cond (null? set)     false
        (= x (car set)) true
        (< x (car set)) false
        :else           (recur x (cdr test))))

(defn adjoin-sorted-set [x set]
  (cond (null? set)     (list x)
        (= x (car set)) set
        (< x (car set)) (cons x set)
        :else           (cons (car set)
                              (adjoin-sorted-set x (cdr set)))))

;;; Exercise 2.62

(defn union-sorted-set [set1 set2]
  (cond (null? set1) set2
        (null? set2) set1
        :else
        (let [[[x1] [x2]]
              [set1 set2]]
          (cond (= x1 x2)
                (recur (cdr set1) set2)
                ;;
                (< x1 x2)
                (cons x1 (union-sorted-set (cdr set1) set2))
                ;;
                (> x1 x2)
                (cons x2 (union-sorted-set set1 (cdr set2)))))))

;;; Exercise 2.67

;; Leaves

(defn make-leaf [symbol weight]
  (list :leaf symbol weight))

(defn leaf? [[tag]] (= tag :leaf))

(defn symbol-leaf [[_ x]] x)

(defn weight-leaf [[_ _ x]] x)

;; Huffman tree

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (let [[_ _ sym] tree] sym)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (let [[_ _ _ w] tree] w)))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(def left-branch  car)
(def right-branch cadr)

;; Decode

(defn choose-branch [bit branch]
  (case bit
    0 (left-branch  branch)
    1 (right-branch branch)
    (print-str "bad bit -- CHOOSE-BRANCH" bit)))

(defn decode [bits tree]
  (letfn
    [(decode-1 [bits current-branch]
               (if (null? bits)
                 '()
                 (let [next-branch
                       (choose-branch (car bits)
                                      current-branch)]
                   (if (leaf? next-branch)
                     (cons (symbol-leaf next-branch)
                           (decode-1 (cdr bits) tree))
                     (recur (cdr bits) next-branch)))))]
    (decode-1 bits tree)))

;; Set of weighted elements

(defn adjoin-weighted-set [x set]
  (cond (null? set)
        (list x)
        ;;
        (< (weight x) (weight (car set)))
        (cons x set)
        ;;
        :else (cons (car set)
                    (adjoin-weighted-set x (cdr set)))))

(defn make-leaf-set [pairs]
  (if (null? pairs)
    '()
    (let [pair (car pairs)]
      (adjoin-weighted-set (make-leaf (car  pair)  ; symbol
                                      (cadr pair)) ; frequency
                           (make-leaf-set (cdr pairs))))))

;;; Exercise 2.68

(defn encode-symbol [sym tree]
  (if (leaf? tree)
    '()
    (let [left  (left-branch  tree)
          right (right-branch tree)]
      (cond (element-of-set? sym (symbols left))
            (cons 0 (encode-symbol sym left))
            ;;
            (element-of-set? sym (symbols right))
            (cons 1 (encode-symbol sym right))
            ;;
            :else
            (print-str "bad symbol -- ENCODE-SYMBOL" sym)))))

(defn encode [message tree]
  (if (null? message)
    '()
    (concat (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

;;; Exercise 2.69

(defn successive-merge [set]
  (if-not (seq (cdr set))
    (car set)
    (let [[min1 min2 & remain] set]
      (recur (adjoin-weighted-set (make-code-tree min1 min2)
                                  remain)))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

;;; Exercise 2.71

;;; 1 bit is required to encode the most frequent symbol;
;;; n bits for the least frequent symbol.

;;; Exercise 2.72

;;; To encode the most frequent symbol:  O(n)
;;; To encode the least frequent symbol: O(n^2)

;;; `get` & `put`

(defn compare-symbol-seq [seq1 seq2]
  (cond (every? empty? [seq1 seq2]) 0
        (empty? seq1)               -1
        (empty? seq2)               1
        :else
        (let [sym1 (->> seq1 first str)
              sym2 (->> seq2 first str)]
          (if (= sym1 sym2)
            (recur (rest seq1) (rest seq2))
            (compare sym1 sym2)))))

(def ^:dynamic *dispatch-table*
  (atom (sorted-map-by compare-symbol-seq)))

(defn my-put [op type item]
  (swap! *dispatch-table*
         assoc-in
         [(if (symbol? type) (list type) type) op]
         item))

(defn my-get [op type]
  (get-in @*dispatch-table*
          [(if (symbol? type) (list type) type) op]))

;;; Exercise 2.73

(defn same-variable? [& vars]
  (and (every? variable? vars)
       (apply = vars)))

(def operator first)
(def operands rest)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn deriv-data-directed [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        :else ((my-get :deriv (operator exp))
               (operands exp) var)))

;; sum & multiply

(defn install-deriv-sum-package []
  (letfn
    [(make-sum [a1 a2]
               (cond (=number? a1 0) a2
                     (=number? a2 0) a1
                     (and (number? a1) (number? a2)) (+ a1 a2)
                     :else (list '+ a1 a2)))
     (addend [[exp]]   exp)
     (augend [[_ exp]] exp)
     (deriv  [exp var] (make-sum (deriv-data-directed (addend exp) var)
                                 (deriv-data-directed (augend exp) var)))]
    (my-put :deriv    '(+) deriv)
    (my-put :make-sum '(+) make-sum)
    :done))

(defn install-deriv-product-package []
  (letfn
    [(make-product [m1 m2]
                   (cond (or (=number? m1 0) (=number? m2 0)) 0
                         (=number? m1 1) m2
                         (=number? m2 1) m1
                         (and (number? m1) (number? m2)) (* m1 m2)
                         :else (list '* m1 m2)))
     (multiplier   [[exp]]   exp)
     (multiplicand [[_ exp]] exp)
     (deriv [exp var]
            (make-sum
             (make-product (multiplier exp)
                           (deriv-data-directed (multiplicand exp) var))
             (make-product (deriv-data-directed (multiplier exp) var)
                           (multiplicand exp))))]
    (my-put :deriv        '(*) deriv)
    (my-put :make-product '(*) make-product)
    :done))

(do
  (install-deriv-sum-package)
  (install-deriv-product-package)
  :done)

(def make-sum     (my-get :make-sum     '(+)))
(def make-product (my-get :make-product '(*)))

;; insert `exponentiation`

(defn install-deriv-exponent-package []
  (letfn
    [(make-exponent
      [base exp]
      (cond (and (number? base)
                 (number? exp)) (expt base exp)
            (=number? exp  0)   1
            (=number? exp  1)   base
            :else (list '** base exp)))
     (base [[exp]] exp)
     (exponent [[_ exp]] exp)
     (deriv [exp var]
            (let [base (base exp)
                  exp (exponent exp)]
              (make-product
               exp
               (make-product (make-exponent base (dec exp))
                             (deriv-data-directed base var)))))]
    (my-put :deriv         '(**) deriv)
    (my-put :make-exponent '(**) make-exponent)
    :done))

(do
  (install-deriv-exponent-package)
  :done)

(def make-exponent (my-get :make-exponent '(**)))

;;; Exercise 2.75

(defn apply-generic-msg-passing [op arg] (arg op))

(defn make-from-mag-ang-msg-passing [r a]
  (letfn
    [(dispatch [op]
               (cond (= op :magnitude) r
                     (= op :angle)     a
                     (= op :real-part) (* r (cos a))
                     (= op :imag-part) (* r (sin a))
                     :else (prn "Unknown op: MAKE-FROM-MAG-ANG" op)))]
    dispatch))

;;; Exercise 2.79

;;; tag selector and operations

(def type-tag   first)
(def contents   second)
(def attach-tag (fn [t o] [t o]))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc      (my-get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (print-str
       "No method for these types: APPLY-GENERIC"
       (list op type-tags)))))

;;; basic arithmetic

(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))

;;; scheme number package

(defn install-scheme-number-package []
  (letfn
    [(tag [x] (attach-tag 'scheme-number x))]
    (my-put :add  '(scheme-number scheme-number) #(tag (+ %1 %2)))
    (my-put :sub  '(scheme-number scheme-number) #(tag (- %1 %2)))
    (my-put :mul  '(scheme-number scheme-number) #(tag (* %1 %2)))
    (my-put :div  '(scheme-number scheme-number) #(tag (/ %1 %2)))
    (my-put :make 'scheme-number                 #(tag %))
    :done))

(defn make-scheme-number [n]
  ((my-get :make 'scheme-number) n))

;;; rational number package

(defn install-rational-package []
  (letfn
    [(numer [x] (first  x))
     (denom [x] (second x))
     (make-rat [n d]
               (let [g (gcd n d)]
                 [(/ n g) (/ d g)]))
     (add-rat [x y]
              (make-rat (+ (* (numer x) (denom y))
                           (* (numer y) (denom x)))
                        (* (denom x) (denom y))))
     (sub-rat [x y]
              (make-rat (- (* (numer x) (denom y))
                           (* (numer y) (denom x)))
                        (* (denom x) (denom y))))
     (mul-rat [x y]
              (make-rat (* (numer x) (numer y))
                        (* (denom x) (denom y))))
     (div-rat [x y]
              (make-rat (* (numer x) (denom y))
                        (* (denom x) (numer y))))
     (tag [x] (attach-tag 'rational x))]
    (my-put :add  '(rational rational) #(tag (add-rat  %1 %2)))
    (my-put :sub  '(rational rational) #(tag (sub-rat  %1 %2)))
    (my-put :mul  '(rational rational) #(tag (mul-rat  %1 %2)))
    (my-put :div  '(rational rational) #(tag (div-rat  %1 %2)))
    (my-put :make 'rational            #(tag (make-rat %1 %2)))
    :done))

(defn make-rational [n d]
  ((my-get :make 'rational) n d))

;;; rectangular package

(defn install-rectangular-package []
  (letfn
    [(real-part [z] (first  z))
     (imag-part [z] (second z))
     (make-from-real-imag [x y] [x y])
     (magnitude [z]
                (sqrt (+ (square (real-part z))
                         (square (imag-part z)))))
     (angle [z]
            (atan2 (imag-part z) (real-part z)))
     (make-from-mag-ang [r a]
                        [(* r (cos a)) (* r (sin a))])
     (tag [x] (attach-tag 'rectangular x))]
    (my-put :real-part 'rectangular real-part)
    (my-put :imag-part 'rectangular imag-part)
    (my-put :magnitude 'rectangular magnitude)
    (my-put :angle     'rectangular angle)
    (my-put :make-from-real-imag 'rectangular
            #(tag (make-from-real-imag %1 %2)))
    (my-put :make-from-mag-ang 'rectangular
            #(tag (make-from-mag-ang) %1 %2))
    :done))

;;; polar package

(defn install-polar-package []
  (letfn
    [(magnitude [z] (first  z))
     (angle     [z] (second z))
     (make-from-mag-ang [r a] [r a])
     (real-part [z]
                (* (magnitude z) (cos (angle z))))
     (imag-part [z]
                (* (magnitude z) (sin (angle z))))
     (make-from-real-imag [x y]
                          [(sqrt (+ (square x) (square y)))
                           (atan2 y x)])
     (tag [x] (attach-tag 'polar x))]
    (my-put :real-part 'polar real-part)
    (my-put :imag-part 'polar imag-part)
    (my-put :magnitude 'polar magnitude)
    (my-put :angle 'polar angle)
    (my-put :make-from-real-imag 'polar
            #(tag (make-from-real-imag %1 %2)))
    (my-put :make-from-mag-ang 'polar
            #(tag (make-from-mag-ang %1 %2)))
    :done))

(defn real-part [z] (apply-generic :real-part z))
(defn imag-part [z] (apply-generic :imag-part z))
(defn magnitude [z] (apply-generic :magnitude z))
(defn angle     [z] (apply-generic :angle     z))

(defn make-from-real-imag [x y]
  ((my-get :make-from-real-imag 'rectangular) x y))

(defn make-from-mag-ang [r a]
  ((my-get :make-from-mag-ang 'polar) r a))

;;; complex number package

(defn install-complex-package []
  (letfn
    [(make-from-real-imag
      [x y]
      ((my-get :make-from-real-imag 'rectangular) x y))
     (make-from-mag-ang
      [r a]
      ((my-get :make-from-mag-ang 'polar) r a))
     (add-complex
      [z1 z2]
      (make-from-real-imag (+ (real-part z1) (real-part z2))
                           (+ (imag-part z1) (imag-part z2))))
     (sub-complex
      [z1 z2]
      (make-from-real-imag (- (real-part z1) (real-part z2))
                           (- (imag-part z1) (imag-part z2))))
     (mul-complex
      [z1 z2]
      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
     (div-complex
      [z1 z2]
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2))))
     (tag [z] (attach-tag 'complex z))]
    (my-put :add '(complex complex) #(tag (add-complex %1 %2)))
    (my-put :sub '(complex complex) #(tag (sub-complex %1 %2)))
    (my-put :mul '(complex complex) #(tag (mul-complex %1 %2)))
    (my-put :div '(complex complex) #(tag (div-complex %1 %2)))
    (my-put :make-from-real-imag 'complex
            #(tag (make-from-real-imag %1 %2)))
    (my-put :make-from-mag-ang 'complex
            #(tag (make-from-mag-ang %1 %2)))
    (my-put :real-part 'complex real-part)
    (my-put :imag-part 'complex imag-part)
    (my-put :magnitude 'complex magnitude)
    (my-put :angle     'complex angle)
    :done))

(defn make-complex-from-real-imag [x y]
  ((my-get :make-from-real-imag 'complex) x y))

(defn make-complex-from-mag-ang [r a]
  ((my-get :make-from-mag-ang 'complex) r a))

(do
  (install-scheme-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  :done)

;;; coercion

(def ^:dynamic *coercion-table* (atom {}))

(defn put-coercion [t1 t2 item]
  (swap! *coercion-table* assoc-in [t1 t2] item))

(defn get-coercion [t1 t2]
  (get-in @*coercion-table* [t1 t2]))

(do
  (my-put :equ? '(scheme-number scheme-number) =)
  (my-put :equ? '(rational rational)
          #(and (= (first  %1) (first  %2))
                (= (second %1) (second %2))))
  (my-put :equ? '(complex complex)
          #(and (= (real-part %1) (real-part %2))
                (= (imag-part %1) (imag-part %2))))
  :done)

(defn equ? [x y] (apply-generic :equ? x y))

;;; Exercise 2.82

(defn apply-generic-coercion [op & args]
  (let
    [type-tags (map type-tag args)
     proc      (my-get op type-tags)]
    ;;
    (letfn
      [(coerce-to-type
        [coll type]
        (cond (nil? (seq coll))
              '()
              ;;
              (= (type-tag (first coll)) type)
              (cons (first coll)
                    (coerce-to-type (rest coll) type))
              ;;
              :else
              (let
                [t1->t2 (get-coercion (type-tag (first coll)) type)]
                (if t1->t2
                  (do
                    (cons (t1->t2 (first coll))
                          (coerce-to-type (rest coll) type)))
                  (cons (first coll)
                        (coerce-to-type (rest coll) type))))))
       ;;
       (apply-coercion
        [coll]
        (loop [head coll]
          (if-not (seq head)
            (print-str "No method for these types"
                       (list op type-tags))
            (let [coerced-list
                  (coerce-to-type coll (type-tag (first head)))
                  proc
                  (my-get op (map type-tag coerced-list))]
              (if proc
                (apply proc (map contents coerced-list))
                (recur (rest head)))))))]
      ;; body
      (if proc
        (apply proc (map contents args))
        (apply-coercion args)))))

(defn scheme-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))

(defn add-complex [z1 z2]
  (make-complex-from-real-imag (+ (real-part z1) (real-part z2))
                               (+ (imag-part z1) (imag-part z2))))

(do
  (put-coercion 'scheme-number
                'complex
                scheme-number->complex)
  (my-put 'add+
          '(complex complex complex)
          #(add-complex (add-complex %1 %2) %3))
  :done)
