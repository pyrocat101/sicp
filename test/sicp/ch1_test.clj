(ns sicp.ch1_test
  (:require [clojure.test :refer :all]
            [sicp.ch1 :refer :all]
            [sicp.test_helper :refer (≈)]))

(deftest test-newton-integral
  (is (≈ 0.25
         (newton-integral cube 0 1 0.01)))
  (is (≈ 0.25
         (newton-integral cube 0 1 0.001))))

(deftest test-simpson-integral
  (is (= 1/4 (simpson-integral cube 0 1 100)))
  (is (= 1/4 (simpson-integral cube 0 1 1000))))

(deftest test-factorial
  (is (= 3628800 (factorial 10))))

(deftest test-john-wallis
  (is (≈ 3.14
         (* 4 (john-wallis 100))
         0.01)))

(deftest test-production-of-relative-primes
  (is (= 189
         (production-of-relative-primes 10))))

(deftest test-fixed-point
  (is (≈ 1.618
         (fixed-point #(+ 1 (/ 1 %)) 1.0)
         0.001)))

(deftest test-fixed-point'
  (is (≈ 4.5555
         (fixed-point' #(/ (log 1000) (log %)) 4)
         0.0001))
  (is (≈ 4.5555
         (fixed-point'
          #(average % (/ (log 1000) (log %))) 4)
         0.0001)))

(deftest test-cont-frac
  (is (≈ 0.618
         (cont-frac (fn [x] 1.0) (fn [x] 1.0) 30)
         0.001)))

(deftest test-iterative-cont-frac
  (is (≈ 0.618
         (iterative-cont-frac
          (fn [x] 1.0) (fn [x] 1.0) 30)
         0.001)))

(deftest test-natural-logarithm
  (is (≈ Math/E natural-logarithm)))

(deftest test-newtons-method
  (is (≈ 1
         (newtons-method (cubic 1 1 -3) 0.0))))

(deftest test-my-double
  (is (= 21
         (((my-double
            (my-double my-double)) inc) 5))))

(deftest test-compose
  (is (= 49 ((compose square inc) 6))))

(deftest test-repeated
  (is (= 625 ((repeated square 2) 5))))

(deftest test-smooth
  (is (≈ 25 ((smooth square) 5))))

(deftest test-repeated-smooth
  (is (≈ 25 ((repeated-smooth square 10) 5))))

(deftest test-damped-nth-root
  (is (≈ (Math/sqrt 2)
         (damped-nth-root 2 2 1))))

(deftest test-nth-root
  (is (≈ 3 (nth-root 10 (Math/pow 3 10)))))

(deftest test-fixed-point-improved
  (is (≈ 1.618
         (fixed-point-improved #(+ 1 (/ 1 %)) 1.0)
         0.001)))
