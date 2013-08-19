(ns sicp.test.ch3
  (:require [clojure.math.numeric-tower :refer (sqrt abs)]
            [clojure.contrib.generic.math-functions :refer (log)]
            [clojure.test :refer :all]
            [sicp.ch3 :refer :all]
            [sicp.test.helper :refer (≈)]))

(deftest test-make-accumulator
  (let [acc (make-accumulator 5)]
    (is (= 15 (acc 10)))
    (is (= 25 (acc 10)))))

(deftest test-make-monitored
  (let [mon (make-monitored sqrt)]
    (mon 100)
    (is (= 1 (mon :how-many-calls)))
    (is (= 0 (mon :reset-count)))
    (is (= 0 (mon :how-many-calls)))))

(deftest test-make-account
  (let [acc (make-account 100 :secret-password)]
    (is (= 60
           ((acc :secret-password :withdraw) 40)))
    (is (= "Incorrect password"
           ((acc :some-other-password :deposit) 50)))))

(deftest test-mento-carlo-estimate-pi
  (is (≈ Math/PI
         (mento-carlo-estimate-pi 100000)
         0.01)))

(deftest test-make-joint
  (is (= 50
         ((peter-acc :open-sesame :withdraw) 50)))
  (is (= 90
         ((paul-acc :rosebud :deposit) 40))))

(deftest test-cycle?
  (is (cycle? (cycle '(666 777))))
  (is (not (cycle? '(666 666 666 666)))))

(deftest test-stream-map-ext
  (let
    [one-to-three  (stream-enumerate-interval 1 3)
     two-to-four   (stream-enumerate-interval 2 4)
     three-to-five (stream-enumerate-interval 3 5)]
    (is (= "(6 9 12)"
           (display-stream
            (stream-map-ext +
                            one-to-three
                            two-to-four
                            three-to-five))))))

(deftest test-factorials
  (is (= '(1 2 6 24 120)
         (take 5 factorials))))

(deftest test-partial-sum
  (is (= '(1 3 6 10 15)
         (take 5 (partial-sums (drop 1 (range)))))))

(deftest test-hamming-numbers
  (is (= '(1 2 3 4 5 6 8 9 10 12 15 16 18 20)
         (take 14 hamming-numbers))))

(deftest test-integrate-series
  (is (= '(1 1/2 1/3 1/4)
         (integrate-series '(1 1 1 1)))))

(deftest test-exp-series
  (is (= (take 100 (integrate-series exp-series))
         (take 100 (rest exp-series)))))

(deftest test-sine-cosine-series
  (is (= (take 10
               (cons 0 (integrate-series cosine-series)))
         (take 10 sine-series))))

(deftest test-mul-series
  (let
    [result
     (map +
          (mul-series cosine-series cosine-series)
          (mul-series sine-series   sine-series))]
    (is (= (first result) 1))
    (is (every? zero? (->> result (drop 1) (take 100))))))

(deftest test-tane-series
  (is (= '(0 1 0N 1/3 0N 2/15 0N 17/315 0N 62/2835)
         (take 10 tane-series))))

(deftest test-stream-limit
  (is (= 1/10
         (stream-limit
          (map / (repeat 1) (drop 1 (range))) 0.012))))

(deftest test-ln2-converge
  (let
    [ln2   (log 2)
     base  (nth ln2-stream 5)
     euler (nth (euler-transform ln2-stream) 5)
     acc   (nth (accelerated-sequence euler-transform
                                      ln2-stream)
                5)]
    (is (> (abs (- base  ln2))
           (abs (- euler ln2))
           (abs (- acc   ln2))))))

