;;;; 2.4 Multiple Representations for Abstract Data
;;;; ==============================================

;;; Namespace and dependencies

(ns sicp.2-4)


;;; `get` & `put`

(def *dispatch-table* (atom {}))

(defn put [op type item]
  (swap! *dispatch-table* assoc-in [type op] item))

(defn get [op type]
  (get-in (deref *dispatch-table*) [type op]))


;;; Exercise 2.75

(def variable? symbol?)
(defn same-variable? [& vars]
  (and (every? variable? vars)
       (apply = vars)))

(def operator first)
(def operands rest)

(defn deriv [exp var]
  (cond (number? exp) 0
        ;;
        (variable? exp)
        (if (same-variable? exp var) 1 0)
        ;;
        :else ((get 'deriv (operator exp))
               (operands exp) var)))

;; sum & multiply

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn install-deriv-sum-package []
  (letfn
    [(make-sum [a1 a2]
               (cond (=number? a1 0) a2
                     (=number? a2 0) a1
                     (and (number? a1) (number? a2)) (+ a1 a2)
                     :else (list '+ a1 a2)))
     (addend [[exp]]   exp)
     (augend [[_ exp]] exp)
     (deriv' [exp var]
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))]
    (put 'deriv    '+ deriv')
    (put 'make-sum '+ make-sum)
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
     (deriv' [exp var]
             (make-sum (make-product (multiplier exp)
                                     (deriv (multiplicand exp) var))
                       (make-product (deriv (multiplier exp) var)
                                     (multiplicand exp))))]
    (put 'deriv        '* deriv')
    (put 'make-product '* make-product)
    :done))

;; try it out
(do
  (install-deriv-sum-package)
  (install-deriv-product-package)
  :done)

(def make-sum     (get 'make-sum '+))
(def make-product (get 'make-product '*))

(deriv '(* (* x y) (+ x 3)) 'x)

;; insert `exponentiation`

(defn install-deriv-exponent-package []
  (letfn
    [(make-exponent [base exp]
                    (cond (and (number? base)
                               (number? exp)) (math/pow base exp)
                          (=number? exp  0)   1
                          (=number? exp  1)   base
                          :else (list '** base exp)))
     (base     [[exp]]   exp)
     (exponent [[_ exp]] exp)
     (deriv' [exp var]
             (let [base (base     exp)
                   exp  (exponent exp)]
               (make-product exp
                             (make-product (make-exponent base (dec exp))
                                           (deriv base var)))))]
    (put 'deriv         '** deriv')
    (put 'make-exponent '** make-exponent)
    :done))

(do
  (install-deriv-exponent-package)
  :done)

(def make-exponent (get 'make-exponent '**))

;; try exponentiation
(deriv '(** x 3) 'x)


;;; Exercise 2.75

(defn apply-generic [op arg] (arg op))

(defn make-from-mag-ang [r a]
  (letfn
    [(dispatch [op]
               (cond (= op 'magnitude) r
                     (= op 'angle)     a
                     (= op 'real-part) (* r (Math/cos a))
                     (= op 'imag-part) (* r (Math/sin a))
                     :else (prn "Unknown op: MAKE-FROM-MAG-ANG" op)))]
  dispatch))

;; try it out
(def x (make-from-mag-ang 5 (Math/atan2 3 4)))
(apply-generic 'magnitude x)
(apply-generic 'angle x)
(apply-generic 'real-part x)
(apply-generic 'imag-part x)
