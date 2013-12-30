;;;; Chapter 1: Building Abstractions with Procedures
;;;; ================================================

;;; Namespace and dependencies

(ns sicp.ch1
  (:use [sicp.utils]))

(defn square [x] (expt x 2))
(defn cube [x] (expt x 3))
(defn average [& lst] (/ (reduce + lst) (count lst)))

;;; Newton Integral

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn newton-integral [f a b dx]
  (let [add-dx #(+ % dx)]
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

;;; Exercise 1.29

(defn simpson-integral [f a b n]
  (let [h    (/ (- b a) n)
        y    (fn [k] (f (+ a (* k h))))
        next (fn [k] (+ k 2))
        term (fn [n] (+ (y (dec n))
                       (* 4 (y n))
                       (y (inc n))))]
    (* (/ h 3) (sum term 1 next (dec n)))))

;;; Exercise 1.30

(defn iterative-sum [term a next b]
  (loop [a a
         result 0]
    (if (> a b)
      result
      (recur (next a)
             (+ (term a) result)))))

;;; Exercise 1.31

;;; a)

(defn product [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(defn factorial [n]
  (product identity 1 inc n))

(defn john-wallis [n]
  (product #(/ (* (inc %) (dec %))
               (square %))
           3.0
           #(+ % 2)
           (+ 1 (* n 2))))

;;; b)

(defn iterative-product [term a next b]
  (loop [a a
         result 1]
    (if (> a b)
      result
      (recur (next a)
             (* (term a) result)))))

;;; Exercise 1.32

(defn accumulate
  [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner
                          null-value
                          term
                          (next a)
                          next b))))

(defn accumulative-sum
  [term a next b]
  (accumulate + 0 term a next b))

(defn accumulative-product
  [term a next b]
  (accumulate * 1 term a next b))

;;; Exercise 1.33

(defn filtered-accumulate
  [combiner null-value term a next b predicate?]
  (if (> a b)
    null-value
    (combiner (let [a' (term a)]
                (if (predicate? a') a' null-value))
              (filtered-accumulate combiner
                                   null-value
                                   term
                                   (next a)
                                   next
                                   b
                                   predicate?))))

;;; b)

(defn production-of-relative-primes [n]
  (letfn [(rel-prime? [x] (= 1 (gcd x n)))]
    (filtered-accumulate * 1 identity 1 inc (dec n) rel-prime?)))

;;; Exercise 1.35

(defn fixed-point [f first-guess]
  (let [tolerance 0.00001
        close-enough? #(≈ %1 %2 tolerance)]
    (loop [guess first-guess]
      (let [next (f guess)]
        (if (close-enough? guess next)
          next
          (recur next))))))

;;; Exercise 1.36

(defn fixed-point-1 [f first-guess]
  (let [tolerance 0.00001
        close-enough? (fn [v1 v2]
                        (< (abs (- v1 v2))
                           tolerance))]
    (loop [guess first-guess steps 0]
      (let [next (f guess)]
        (if (close-enough? guess next)
          next
          (recur next (inc steps)))))))

;;; Exercise 1.37

;;; a)

(defn cont-frac [n d k]
  (letfn [(f [i]
            (let [next (inc i)]
              (if (> i k)
                0
                (/ (n i) (+ (d i) (f next))))))]
    (f 1)))

;;; b)

(defn iterative-cont-frac [n d k]
  (loop [i k result 0]
    (if (< i 1)
      result
      (recur (dec i)
             (/ (n i) (+ (d i) result))))))

(iterative-cont-frac (fn [x] 1.0) (fn [x] 1.0) 30)

;;; Exercise 1.38

(def natural-logarithm
  (letfn [(n [_] 1.0)
          (d [i]
            (if (= 0 (mod (inc i) 3))
              (* 2 (/ (inc i) 3))
              1))]
    (+ 2 (iterative-cont-frac n d 20))))

;;; Exercise 1.40

(defn deriv [g]
  (let [dx 0.00001]
    (fn [x]
      (/ (- (g (+ x dx))
            (g x))
         dx))))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn cubic [a b c]
  (fn [x] (+ (cube x)
            (* a (square x))
            (* b x)
            c)))

;;; Exercise 1.41

(defn my-double [f] #(f (f %)))

;;; Exercise 1.42

(defn compose [f g] #(f (g %)))

;;; Exercise 1.43

(defn repeated [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))

;;; Exercise 1.44

(defn smooth [f]
  (let [dx 0.00001]
    #(average (f %)
              (f (- % dx))
              (f (+ % dx)))))

(defn repeated-smooth [f n] ((repeated smooth n) f))

;;; Exercise 1.45

(defn average-damp [f] #(average % (f %)))
(defn repeated-average-damp [f n] ((repeated average-damp n) f))

(defn damped-nth-root [n x damp-times]
  (let [f #(/ x (expt % (dec n)))]
    (fixed-point (repeated-average-damp f damp-times)
                 1.0)))

;;; damp-times is lg(n)

(defn lg [n] (ceil (/ (log n) (log 2))))

(defn nth-root [n x]
  (let [damp-times (int (lg n))]
    (damped-nth-root n x damp-times)))

;;; Exercise 1.46

(defn iterative-improve [good-enough? improve]
  (fn [first-guess]
    (loop [result first-guess]
      (let [next (improve result)]
        (if (good-enough? result next)
          next
          (recur next))))))

(defn fixed-point-improved [f first-guess]
  (letfn [(good-enough? [v1 v2]
            (< (abs (- v1 v2))
               0.00001))
          (improve [guess] (f guess))]
    ((iterative-improve good-enough? improve) first-guess)))
