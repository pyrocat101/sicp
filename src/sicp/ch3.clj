;;;; Chapter 3: Modularity, Objects, and State
;;;; =========================================

;;; Namespace and dependencies

(ns sicp.ch3
  (:require [clojure.contrib.generic.math-functions
             :refer (abs sqr sin pow)]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(defn ^boolean divisible?
  [^long dividend ^long divisor]
  (zero? (rem dividend divisor)))

;;; https://gist.github.com/michalmarczyk/486880

(defmacro letrec [bindings & body]
  (let [bcnt (quot (count bindings) 2)
        arrs (gensym "bindings_array")
        arrv `(make-array Object ~bcnt)
        bprs (partition 2 bindings)
        bssl (map first bprs)
        bsss (set bssl)
        bexs (map second bprs)
        arrm (zipmap bssl (range bcnt))
        btes (map #(walk/prewalk (fn [f]
                                   (if (bsss f)
                                     `(aget ~arrs ~(arrm f))
                                     f))
                                 %)
                  bexs)]
    `(let [~arrs ~arrv]
       ~@(map (fn [s e]
                `(aset ~arrs ~(arrm s) ~e))
              bssl
              btes)
       (let [~@(mapcat (fn [s]
                         [s `(aget ~arrs ~(arrm s))])
                       bssl)]
         ~@body))))

;;; Exercise 3.1

(defn make-accumulator [init]
  (let [value (atom init)]
    (fn [delta] (swap! value + delta))))

;;; Exercise 3.2

(defn make-monitored [f]
  (let [counter (atom 0)]
    (fn [& args]
      (cond (= [:how-many-calls] args) @counter
            (= [:reset-count]    args) (reset! counter 0)
            :else (do
                    (swap! counter inc)
                    (apply f args))))))

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

;;; Exercise 3.5

(defn ^double random-in-range
  [^double low ^double high]
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

(defn mento-carlo-estimate-pi [^long times]
  (/ (estimate-integral
      (fn [x y] (>= 9 (+ (sqr (- x 5))
                         (sqr (- y 7)))))
      {:x1 2, :x2 8, :y1 4, :y2 10}
      times)
     9))

;;; Exercise 3.7

(defn make-joint [account password new-password]
  (fn [p m]
    (if-not (= p new-password)
      (fn [& args] "Incorrect password")
      (account password m))))

(def peter-acc (make-account 100 :open-sesame))
(def paul-acc  (make-joint peter-acc :open-sesame :rosebud))

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

;;; Exercise 3.51

;;; The interpreter will print numbers from 0 to 7.

;;; Exercise 3.52

;;; sum: 1 -> 3 -> 15 -> 36 -> 210
;;; no memo-proc (the evaluated result is not cached):
;;; 1 -> 4 -> 19 -> 55 -> 265

;;; Exercise 3.53

;;; The power-of-2 sequence.

;;; Exercise 3.54

(def factorials
  (lazy-seq
   (cons 1 (map * factorials (drop 2 (range))))))

;;; Exercise 3.55

(defn partial-sums [s]
  (lazy-seq
   (cons (first s)
         (map + (partial-sums s) (rest s)))))

;;; Exercise 3.56

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn my-merge [s1 s2]
  (lazy-seq
   (cond (empty? s1) s2
         (empty? s2) s1
         :else
         (let [s1-car (first s1)
               s2-car (first s2)]
           (cond (< s1-car s2-car)
                 (cons s1-car (my-merge (rest s1) s2))
                 (> s1-car s2-car)
                 (cons s2-car (my-merge s1 (rest s2)))
                 :else
                 (cons s1-car
                       (my-merge (rest s1) (rest s2))))))))

(def hamming-numbers
  (lazy-seq
   (cons 1 (my-merge
            (map #(* % 2) hamming-numbers)
            (my-merge (map #(* % 3) hamming-numbers)
                      (map #(* % 5) hamming-numbers))))))

;;; Exercise 3.59

(defn integrate-series [s]
  (lazy-seq (map / s (iterate inc 1))))

(def exp-series
  (lazy-seq (cons 1 (integrate-series exp-series))))

(declare cosine-series sine-series)

(def cosine-series
  (lazy-seq
   (cons 1 (map - (integrate-series sine-series)))))

(def sine-series
  (lazy-seq
   (cons 0 (integrate-series cosine-series))))

; Exercise 3.60

(defn mul-series [s1 s2]
  (lazy-seq
   (let [s1car (first s1)
         s2car (first s2)]
     (cons (* s1car s2car)
           (map +
                (map #(* % s1car) (rest s2))
                (mul-series (rest s1) s2))))))

;;; Exercise 3.61

(defn reciprocal-series [s]
  (letrec
   [result
    (lazy-seq
     (cons 1 (mul-series (map - (rest s)) result)))]
   result))

;;; Exercise 3.62

(defn div-series [s1 s2]
  (let [s2car (first s2)]
    (if (= s2car 0)
      "divide by zero"
      (mul-series
       s1
       (reciprocal-series (map #(/ % s2car) s2))))))

(def tane-series (div-series sine-series cosine-series))

;;; Exercise 3.63

;;; Though `(sqrt-stream x)` recursively called inside
;;; `(sqrt-stream x)`, these two streams are not the same
;;; one. Therefore rebundant computation is performed.
;;;
;;; If `memo-proc` is not used, the two versions are essentially
;;; the same in terms of efficiency.

;;; Exercise 3.64

(defn stream-limit [[a b :as s] tolerance]
  (if (< (abs (double (- b a))) tolerance)
    b
    (recur (rest s) tolerance)))

;;; Exercise 3.65

(defn ln2-summands [n]
  (lazy-seq
   (cons (/ 1.0 n)
         (map - (ln2-summands (inc n))))))

(defn euler-transform [[s0 s1 s2 :as s]]
  (lazy-seq
   (cons (- s2 (/ (sqr (- s2 s1))
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

;;; Exercise 3.67

(def integers (drop 1 (range)))

(defn pairs-all
  [[s-car & s-cdr :as s]
   [t-car & t-cdr :as t]]
  (lazy-seq
   (cons
    [s-car t-car]
    (interleave
     (map (fn [x] [s-car x]) t-cdr)
     (map (fn [x] [x t-car]) s-cdr)
     (pairs-all s-cdr t-cdr)))))

;;; Exercise 3.68

;;; It doesn't work. The evaluation will result in an
;;; infinite loop of calling `interleave` and `pair`
;;; recursively, because there is no delay.

;;; Exercise 3.69

(defn pairs
  [[s-car & s-cdr :as s]
   [t-car & t-cdr :as t]]
  (lazy-seq
   (cons
    [s-car t-car]
    (interleave
     (map (fn [x] [s-car x]) t-cdr)
     (pairs s-cdr t-cdr)))))

(defn triples
  [[r-car & r-cdr :as r]
   [s-car & s-cdr :as s]
   [t-car & t-cdr :as t]]
  (lazy-seq
   (cons
    [r-car s-car t-car]
    (interleave
     (map (fn [[x y]] [r-car x y])
          (pairs s-cdr t-cdr))
     (triples r-cdr s-cdr t-cdr)))))

(def pythagorean-triples
  (filter
   (fn ^double
     [[^double a ^double b ^double c]]
     (= (sqr c)
        (+ (sqr a) (sqr b))))
   (triples integers integers integers)))

;;; Exercise 3.70

(defn weighted-merge [s1 s2 weight]
  (lazy-seq
   (cond
    (empty? s1) s2
    (empty? s2) s1
    :else
    (let [s1-car (first s1)
          s2-car (first s2)]
      (case (compare (weight s1-car)
                     (weight s2-car))
        -1 (cons s1-car (weighted-merge
                         (rest s1) s2 weight))
        1  (cons s2-car (weighted-merge
                         s1 (rest s2) weight))
        (cons s1-car
              (cons s2-car
                    (weighted-merge
                     (rest s1) (rest s2) weight))))))))

(defn weighted-pairs
  [[s-car & s-cdr :as s]
   [t-car & t-cdr :as t]
   weight]
  (lazy-seq
   (cons
    [s-car t-car]
    (weighted-merge
     (map (fn [x] [s-car x]) t-cdr)
     (weighted-pairs s-cdr t-cdr weight)
     weight))))

;; a)

(def pairs-sorted-1
  (weighted-pairs integers
                  integers
                  (fn [[i j]] (+ i j))))

;; b)

(def pairs-sorted-2
  (let
    [s (filter
        #(and (divisible? % 2)
              (divisible? % 3)
              (divisible? % 5))
        integers)]
    (weighted-pairs s
                    s
                    (fn [[i j]]
                      (+ (* i 2) (* j 3) (* 5 i j))))))

;;; Exercise 3.71

(def ramanujan
  (let
    [f (fn [[i j]] (+ (* i i i) (* j j j)))
     sorted (weighted-pairs integers integers f)
     cube-sum (map f sorted)]
    (->> (map vector cube-sum (rest cube-sum))
         (filter (fn [[x y]] (= x y)))
         (map first))))

;;; Exercise 3.74

(defn sign-change-detector [v1 v2]
  (cond (>= 0 (* v1 v2)) 0
        (< v1 0) -1
        (> v1 0)  1))

(defn zero-crossings [sense-data]
  (map sign-change-detector
       sense-data
       (lazy-seq (cons 0 (rest sense-data)))))

;;; Exercise 3.75

(defn make-zero-crossings
  [input-stream last-avpt last-value]
  (let
    [value (first input-stream)
     avpt  (/ (+ value last-value) 2)]
    (lazy-seq
     (cons (sign-change-detector avpt last-avpt)
           (make-zero-crossings (rest input-stream)
                                avpt
                                value)))))

;;; Exercise 3.76

(defn smooth [s]
  (map (fn [[a b]] (/ (+ a b) 2))
       (partition 2 1 s)))

(defn make-zero-crossings-smoothed
  [input-stream]
  (let [smoothed-data (smooth input-stream)]
    (lazy-seq
     (map sign-change-detector
          smoothed-data
          (lazy-seq (cons 0 (rest smoothed-data)))))))

;;; Exercise 3.82

(defn mento-carlo
  [experiment-stream passed failed]
  (letfn
    [(next
      [passed failed]
      (lazy-seq
       (cons (/ passed (+ passed failed))
             (mento-carlo
              (rest experiment-stream) passed failed))))]
    (if (first experiment-stream)
      (next (inc passed) failed)
      (next passed (inc failed)))))

(defn estimate-integral-stream
  [P {:keys [x1 x2 y1 y2]}]
  (let
    [x-random-stream
     (repeatedly #(+ x1 (* (rand) (- x2 x1))))
     y-random-stream
     (repeatedly #(+ y1 (* (rand) (- y2 y1))))
     integral-stream
     (map #(P %1 %2) x-random-stream y-random-stream)]
    (map #(double (* % (- y2 y1) (- x2 x1)))
         (mento-carlo integral-stream 0 0))))

(defn estimate-pi-integral-stream [times]
  (/ (nth (estimate-integral-stream
           (fn [x y] (>= 9 (+ (sqr (- x 5))
                              (sqr (- y 7)))))
           {:x1 2, :x2 8, :y1 4, :y2 10})
          times) 9))
