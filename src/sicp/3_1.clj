;;;; 3.1 Assignment and Local State
;;;; ==============================

;;; Namespace and dependencies

(ns sicp.3-1
  (:require [clojure.contrib.generic.math-functions :as math]))


;;; Exercise 3.1

(defn make-accumulator [init]
  (let [value (atom init)]
    (fn [delta] (swap! value + delta))))

;; try it out

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

;; try it out

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
       balance)
     dispatch
     (fn [p m]
       (if-not (= p password)
         (fn [& args] "Incorrect password")
         (cond (= m :withdraw) withdraw
               (= m :diposit)  desposit
               :else
               (fn [& args]
                 (print-str
                  "Unknown request -- MAKE-ACCOUNT" m)))))]
    dispatch))

;; try it out

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


;;; Exercise 3.6
























