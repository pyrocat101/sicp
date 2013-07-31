;;;; Chapter 3: Modularity, Objects, and State
;;;; =========================================

;;; Namespace and dependencies

(ns sicp.ch3
  (:require [clojure.contrib.generic.math-functions :as math])
  (:require [clojure.string :as string]))

;;; Exercise 3.1

(defn make-accumulator [init]
  (let [value (atom init)]
    (fn [delta] (swap! value + delta))))

(def A-3-1 (make-accumulator 5))

(A-3-1 10)
(A-3-1 10)

;;; Exercise 3.2

(defn make-monitored [f]
  (let [counter (atom 0)]
    (fn [& args]
      (cond (= [:how-many-calls] args) @counter
            (= [:reset-count]    args) (reset! counter 0)
            :else (do
                    (swap! counter inc)
                    (apply f args))))))

(def s-3-2 (make-monitored math/sqrt))

(s-3-2 100)
(s-3-2 :how-many-calls)
(s-3-2 :reset-count)
(s-3-2 :how-many-calls)

;;; Exercise 3.3

(defn make-account [balance password]
  (let
    [balance (atom balance)
     withdraw
     (fn [amount]
       (if (>= @balance amount)
         (do
           (swap! balance - amount)
           @balance)
         "Insufficient funds"))
     desposit
     (fn [amount]
       (swap! balance + amount)
       @balance)
     dispatch
     (fn [p m]
       (if-not (= p password)
         (fn [& args] "Incorrect password")
         (cond (= m :withdraw) withdraw
               (= m :deposit)  desposit
               :else
               (fn [& args]
                 (print-str
                  "Unknown request -- MAKE-ACCOUNT" m)))))]
    dispatch))

(def acc-3-3 (make-account 100 :secret-password))

((acc-3-3 :secret-password :withdraw) 40)
((acc-3-3 :some-other-password :deposit) 50)

;;; Exercise 3.5

(defn random-in-range [low high]
  (+ low (* (rand) (- high low))))

(defn estimate-integral
  [P {:keys [x1 x2 y1 y2]} times]
  (let [hit (atom 0)]
    (dotimes [_ times]
      (let [x (random-in-range x1 x2)
            y (random-in-range y1 y2)]
        (if (P x y)
          (swap! hit inc))))
    (double (* (/ @hit times) (- y2 y1) (- x2 x1)))))

(defn square [n] (math/pow n 2))

(/ (estimate-integral (fn [x y] (>= 9 (+ (square (- x 5))
                                         (square (- y 7)))))
                      {:x1 2, :x2 8, :y1 4, :y2 10}
                      10000) 9)

;;; Exercise 3.7

(defn make-joint [account password new-password]
  (fn [p m]
    (if-not (= p new-password)
      (fn [& args] "Incorrect password")
      (account password m))))

(def peter-acc (make-account 100 :open-sesame))
(def paul-acc  (make-joint peter-acc :open-sesame :rosebud))

((peter-acc :open-sesame :withdraw) 50)
((paul-acc  :rosebud     :deposit)  40)

;;; Exercise 3.19

(defn cycle? [coll]
  (loop [x (drop 1 coll)
         y (drop 2 coll)]
    (cond (or (nil? (seq x))
              (nil? (seq y)))
          false
          (identical? (first x)
                      (first y))
          true
          :else
          (recur (drop 1 x)
                 (drop 2 y)))))

(cycle? (cycle '(666 777)))
(cycle? '(666 666 666 666))

;;; Exercise 3.47

(defn test-and-set! [cell]
  (dosync
   (if @cell
     true
     (do
       (ref-set cell true)
       false))))

(defn clear! [cell]
  (dosync
   (ref-set cell false)))

(defn make-mutex []
  (let [cell (ref false)]
    (defn the-mutex [m]
      (condp = m
        :acquire (if (test-and-set! cell)
                   (recur :acquire))
        :release (clear! cell)))
    the-mutex))

(defn make-semaphore [n]
  (let [n     (atom n)
        mutex (make-mutex)]
    (defn acquire []
      (mutex :acquire)
      (if (> n 0)
        (do
          (swap! n dec)
          (mutex :release)
          :done)
        (do
          (mutex :release)
          (recur))))
    (defn release []
      (mutex :acquire)
      (swap! n inc)
      (mutex :release)
      :done)
    (defn dispatch [m]
      (condp = m
        :acquire (acquire)
        :release (release)))
    dispatch))

;; `make-semaphore` clojure style

(defn make-semaphore-clj [n]
  (let [n (ref n)]
    (defn acquire []
      (dosync
       (when (> n 0)
         (alter n dec)
         :done)))
    (defn release []
      (dosync
       (alter n inc)
       :done))
    (defn dispatch [m]
      (condp = m
        :acquire (acquire)
        :release (release)))
    dispatch))

;;; Exercise 3.50

(defmacro my-delay [& body]
  `(memoize (fn [] ~@body)))

(defn my-force [delayed-object]
  (delayed-object))

(defmacro cons-stream [a b]
  `[~a (my-delay ~b)])

(def stream-car first)
(def stream-cdr (comp my-force second))

(def the-empty-stream '())
(def stream-null? (comp nil? seq))

(defn stream-nth [s n]
  (if (= n 0)
    (stream-car s)
    (recur (stream-cdr s) (dec n))))

(defn stream-map [proc s]
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(defn stream-reduce [proc acc s]
  (if (stream-null? s)
    acc
    (recur proc
           (proc acc (stream-car s))
           (stream-cdr s))))

(defn display-stream [s]
  (format
   "(%s)"
   (string/join " "
                (reverse (stream-reduce #(cons (str %2) %1)
                                        '() s)))))

(defn stream-enumerate-interval [low high]
  (if (> low high)
    the-empty-stream
    (cons-stream
     low
     (stream-enumerate-interval (inc low) high))))

(defn stream-filter [pred s]
  (cond (stream-null? s) the-empty-stream
        (pred (stream-car s))
        (cons-stream (stream-car s)
                     (stream-filter pred
                                    (stream-cdr s)))
        :else
        (recur pred (stream-cdr s))))

(defn stream-map-ext [proc & argstreams]
  (if (stream-null? (first argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map-ext
            (cons proc (map stream-cdr argstreams))))))

(let
  [one-to-three  (stream-enumerate-interval 1 3)
   two-to-four   (stream-enumerate-interval 2 4)
   three-to-five (stream-enumerate-interval 3 5)]
  (display-stream (stream-map-ext +
                                  one-to-three
                                  two-to-four
                                  three-to-five)))

;;; Exercise 3.51

;;; The interpreter will print numbers from 0 to 7.

;;; Exercise 3.52

;;; sum: 1 -> 3 -> 15 -> 36 -> 210
;;; no memo-proc (the evaluated result is not cached):
;;; 1 -> 4 -> 19 -> 55 -> 265

;;; Exercise 3.53

((fn ! [] (lazy-seq (cons 1 (map + (!) (!))))))

;;; Exercise 3.54

((fn ! [] (lazy-seq (cons 1 (map * (!) (drop 2 (range)))))))

;;; Exercise 3.55

(defn partial-sums [s]
  (lazy-seq
   (cons (first s)
         (map + (partial-sums s) (rest s)))))

(partial-sums (drop 1 (range)))

;;; Exercise 3.56

(def my-merge (comp distinct interleave))

((fn ! []
   (lazy-seq
    (cons 1 (my-merge (map #(* 2 %) (!))
                      (my-merge (map #(* 3 %) (!))
                                (map #(* 5 %) (!))))))))

;;; Exercise 3.59

(defn integrate-series [s]
  (lazy-seq (map / s (iterate inc 1))))

(cons :c (integrate-series '(1 1 1 1)))

;; (def exp-series
;;   (lazy-seq (cons 1 (integrate-series exp-series))))

(declare cosine-series sine-series)

(def cosine-series
  (lazy-seq
   (cons 1 (map - (integrate-series sine-series)))))

(def sine-series
  (lazy-seq
   (cons 0 (integrate-series cosine-series))))

;;; Exercise 3.60

(defn mul-series [s1 s2]
  (lazy-seq
   (let [s1car (first s1)
         s2car (first s2)]
     (cons (* s1car s2car)
           (map +
                (map #(* % s1car) (rest s2))
                (mul-series (rest s1) s2))))))

(map +
     (mul-series cosine-series cosine-series)
     (mul-series sine-series   sine-series))

;;; Exercise 3.61

(defn reciprocal-series [s]
  (def result
    (lazy-seq
     (cons 1 (mul-series (map - (rest s))
                         result))))
  result)

;;; Exercise 3.62

(defn div-series [s1 s2]
  (let [s2car (first s2)]
    (if (= s2car 0)
      "divide by zero"
      (mul-series
       s1
       (reciprocal-series (map #(/ % s2car) s2))))))

;; (def tane-series (div-series sine-series cosine-series))

;;; Exercise 3.63

;;; Though `(sqrt-stream x)` recursively called inside
;;; `(sqrt-stream x)`, these two streams are not the same
;;; one. Therefore rebundant computation is performed.
;;;
;;; If `memo-proc` is not used, the two versions are essentially
;;; the same in terms of efficiency.

;;; Exercise 3.64

(defn stream-limit [[a b :as s] tolerance]
  (if (< (math/abs (double (- b a))) tolerance)
    b
    (recur (rest s) tolerance)))

(stream-limit (map / (repeat 1) (drop 1 (range))) 0.012)

;;; Exercise 3.65

(defn ln2-summands [n]
  (lazy-seq
   (cons (/ 1.0 n)
         (map - (ln2-summands (inc n))))))

(defn euler-transform [[s0 s1 s2 :as s]]
  (lazy-seq
   (cons (- s2 (/ (Math/pow (- s2 s1) 2)
                     (+ s0 (* -2 s1) s2)))
         (euler-transform (rest s)))))

(defn make-tableau [transform s]
  (lazy-seq
   (cons s (make-tableau transform
                         (transform s)))))

(defn accelerated-sequence [transform s]
  (map first
       (make-tableau transform s)))

(def ln2-stream
  (partial-sums (ln2-summands 1)))

(lazy-seq ln2-stream)
(euler-transform ln2-stream)
(accelerated-sequence euler-transform
                      ln2-stream)

;;; Exercise 3.66









