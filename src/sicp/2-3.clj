;;;; 2.3 Symbolic Data
;;;; =================

;;; Namespace and dependencies

(ns sicp.2-3)

;;; Utility functions

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

;;; Exercise 2.54

(defn equal? [list1 list2]
  (cond (or (= list1 '()) (= list2 '()))   true
        (not= (first list1) (first list2)) false
        :else (recur (rest list1) (rest list2))))

(equal? '(a b c d) '(a b c d))
(equal? '(a b c d) '(a c b d))


;;; Exercise 2.55

;;; (car ''abracadabra)
;;; -> (car (quote (quote abracadabra)))
;;; -> quote


;;; Exercise 2.56

(def variable? symbol?)

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

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
(def augend caddr)

(defn product? [x] (and (seq? x) (= (car x) '*)))
(def multiplier   cadr)
(def multiplicand caddr)

(defn exponentiation? [x] (and (seq? x) (= (car x) '**)))
(def base cadr)
(def exponent caddr)

(defn make-exponentiation [base exp]
  (cond (and (number? base)
             (number? exp)) (math/expt base exp)
        (=number? exp  0)   1
        (=number? exp  1)   base
        :else (list '** base exp)))

(defn deriv [exp var]
  (cond
   ;;
   (number? exp) 0
   ;;
   (variable? exp)
   (if (same-variable? exp var) 1 0)
   ;
   (sum? exp)
   (make-sum (deriv (addend exp) var)
             (deriv (augend exp) var))
   ;;
   (product? exp)
   (make-sum
    (make-product (multiplier exp)
                  (deriv (multiplicand exp) var))
    (make-product (deriv (multiplier exp) var)
                  (multiplicand exp)))
   ;;
   (exponentiation? exp)
   (let [base (base     exp)
         exp  (exponent exp)]
     (make-product exp
                   (make-product (make-exponentiation base (dec exp))
                                 (deriv base var))))
   ;;
   :else
   (println "unknown expression type -- DERIV" exp)))

;; try it out
(deriv '(** x 3) 'x)


;;; Exercise 2.57


;; modified version

(defn augend [s]
  (if-not (seq (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(defn multiplicand [p]
  (if-not (seq (cdddr p))
    (caddr p)
    (cons '* (cddr p))))

(deriv '(* x y (+ x 3)) 'x)


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

;; try out
(intersection-dup-set '(2 3 2 1 3 2 2) '(1 2))
(union-dup-set '(2 3 2 1 3 2 2) '(3 4))

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

;; try out
(adjoin-sorted-set 3 '(0 1 2))
(adjoin-sorted-set 3 '(1 2 4))
(adjoin-sorted-set 3 '(4 5 6))


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

;; try out
(union-sorted-set '(1 3) '(2 4))


;;; Exercise 2.67

;; Leaves

(defn make-leaf [symbol weight]
  (list :leaf symbol weight))

(defn leaf? [[tag]] (= tag :leaf))

(defn symbol-leaf [[_ x]] x)

(defn weight-leaf [[_ _ x]] x)

;; Huffman tree

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(def left-branch  car)
(def right-branch cadr)

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (let [[_ _ sym] tree] sym)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (let [[_ _ _ w] tree] w)))

;; decode

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

(defn choose-branch [bit branch]
  (case bit
    0 (left-branch  branch)
    1 (right-branch branch)
    (println "bad bit -- CHOOSE-BRANCH" bit)))

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

;; Sample tree

(def sample-tree
  (make-code-tree (make-leaf :A 4)
                  (make-code-tree (make-leaf :B 2)
                                  (make-code-tree (make-leaf :D 1)
                                                  (make-leaf :C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; try `decode`

(decode sample-message sample-tree)


;;; Exercise 2.68

(defn encode [message tree]
  (if (null? message)
    '()
    (concat (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

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
            (println "bad symbol -- ENCODE-SYMBOL" sym)))))

;; try `encode`

(= sample-message
  (encode (decode sample-message sample-tree) sample-tree))


;;; Exercise 2.69

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

(defn successive-merge [set]
  (if-not (seq (cdr set))
    (car set)
    (let [[min1 min2 & remain] set]
      (recur (adjoin-weighted-set (make-code-tree min1 min2)
                                  remain)))))

;; try it out

(def sample-tree-2
  (generate-huffman-tree '((:A 4) (:B 2) (:D 1) (:C 1))))

(= sample-message
   (encode (decode sample-message sample-tree-2) sample-tree-2))


;;; Exercise 2.71

;;; 1 bit is required to encode the most frequent symbol;
;;; n bits for the least frequent symbol.


;;; Exercise 2.72

;;; To encode the most frequent symbol:  O(n)
;;; To encode the least frequent symbol: O(n^2)
