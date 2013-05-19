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

; Exercise 1.33

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

; b)

(defn production-of-relative-primes [n]
  (letfn [(rel-prime? [x] (= 1 (math/gcd x n)))]
    (filtered-accumulate * 1 identity 1 inc (dec n) rel-prime?)))

(production-of-relative-primes 10)


; Exercise 1.35

(defn fixed-point [f first-guess]
  (let [tolerance 0.00001
        close-enough? (fn [v1 v2]
                        (< (math/abs (- v1 v2))
                           tolerance))
        try' (fn ! [guess]
               (let [next (f guess)]
                 (if (close-enough? guess next)
                   next
                   (! next))))]
    (try' first-guess)))

(fixed-point #(+ 1 (/ 1 %)) 1.0)

; Exercise 1.36

(defn fixed-point' [f first-guess]
  (let [tolerance 0.00001
        close-enough? (fn [v1 v2]
                        (< (math/abs (- v1 v2))
                           tolerance))
        try' (fn ! [guess steps]
               (let [next (f guess)]
                 (if (close-enough? guess next)
                   (do (println "steps:" steps) next)
                   (! next (inc steps)))))]
    (try' first-guess 0)))

; utility functions

(defn log [x] (Math/log x))
(defn average [& lst] (/ (reduce + lst) (count lst)))

; compare steps

(fixed-point' #(/ (log 1000) (log %)) 4)
(fixed-point' #(average % (/ (log 1000) (log %))) 4)

; Exercise 1.37

; a)

(defn cont-frac [n d k]
  (letfn [(f [i]
             (let [next (inc i)]
               (if (> i k)
                 0
                 (/ (n i) (+ (d i) (f next))))))]
    (f 1)))

(cont-frac (fn [x] 1.0) (fn [x] 1.0) 30)

; b)

(defn iterative-cont-frac [n d k]
  (loop [i k, result 0]
    (if (< i 1)
      result
      (recur (dec i)
             (/ (n i) (+ (d i) result))))))

; Test out `iterative-cont-frac`

(iterative-cont-frac (fn [x] 1.0) (fn [x] 1.0) 30)

; Exercise 1.38

(letfn [(n [_] 1.0)
        (d [i]
           (if (= 0 (mod (inc i) 3))
             (* 2 (/ (inc i) 3))
             1))]
  (iterative-cont-frac n d 20))

; Exercie 1.40

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

(newtons-method (cubic 1 1 -3) 0.0)

; Exercise 1.41

(defn double [f] #(f (f %)))

(((double (double double)) inc) 5)

; Exercise 1.42

(defn compose [f g] #(f (g %)))

((compose square inc) 6)

; Exercise 1.43

(defn repeated [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))

((repeated square 2) 5)

; Exercise 1.44

(defn smooth [f]
  (let [dx 0.00001]
    #(average (f %)
              (f (- % dx))
              (f (+ % dx)))))

((smooth square) 5)

(defn repeated-smooth [f n] ((repeated smooth n) f))

((repeated-smooth square 10) 5)

; Exercise 1.45

(defn average-damp [f] #(average % (f %)))
(defn repeated-average-damp [f n] ((repeated average-damp n) f))

(defn damped-nth-root [n x damp-times]
  (let [f #(/ x (math/expt % (dec n)))]
    (fixed-point (repeated-average-damp f damp-times)
                 1.0)))

(damped-nth-root 2 2 1)

; damp-times is lg(n)

(defn lg [n] (math/ceil (/ (log n) (log 2))))

(defn nth-root [n x]
  (let [damp-times (int (lg n))]
    (damped-nth-root n x damp-times)))

(nth-root 10 (math/expt 3 10))

; Exercise 1.46

(defn iterative-improve [good-enough? improve]
  (fn [first-guess]
    (loop [result first-guess]
      (let [next (improve result)]
        (if (good-enough? result next)
          next
          (recur next))))))

(defn fixed-point-improved [f first-guess]
  (letfn [(good-enough? [v1 v2]
                        (< (math/abs (- v1 v2))
                           0.00001))
          (improve [guess] (f guess))]
    ((iterative-improve good-enough? improve) first-guess)))

(fixed-point-improved #(+ 1 (/ 1 %)) 1.0)
