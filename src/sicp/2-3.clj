;;;; 2.3 Symbolic Data
;;;; =================

;;; Namespace and dependencies

(ns sicp.2-3
  (:require [clojure.math.numeric-tower :as math]))

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























