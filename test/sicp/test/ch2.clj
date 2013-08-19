(ns sicp.test.ch2
  (:require [clojure.test :refer :all]
            [sicp.ch2 :refer :all]
            [sicp.test.helper :refer (≈)]))

(deftest test-rat
  (is (= "-1/2"
         (print-rat (make-rat 2 -4)))))

(deftest test-point
  (is (= "(1, 1)"
         (print-point
          (midpoint-segment
           (make-segment (make-point 0 0)
                         (make-point 2 2)))))))

(deftest test-cons'-car'-cdr'
  (is (= :x (car' (cons' :x :y))))
  (is (= :y (cdr' (cons' :x :y)))))

(deftest test-cons''-car''-cdr''
  (is (= 4 (car'' (cons'' 4 5))))
  (is (= 5 (cdr'' (cons'' 4 5)))))

(deftest test-sub-interval
  (is (= [-3 3]
         (sub-interval (make-interval 1 3)
                       (make-interval 0 4)))))

(deftest test-percent
  (is (≈ 0.1
         (percent (make-center-percent 3.5 0.1)))))

(deftest test-last-pair
  (is (= '(34)
         (last-pair (list 23 72 149 34)))))

(deftest test-reverse'
  (is (= '() (reverse' '())))
  (let [l (list 1 4 9 16 25)]
    (is (= (reverse l) (reverse' l)))))

(deftest test-change-coins
  (is (= 292
         (cc 100 us-coins)
         (cc 100 (reverse us-coins))))
  (is (= 4563
         (cc 100 uk-coins)
         (cc 100 (reverse uk-coins)))))

(deftest test-same-parity
  (is (= '(1 3 5 7)
         (same-parity 1 2 3 4 5 6 7)))
  (is (= '(2 4 6)
         (same-parity 2 3 4 5 6 7))))

(deftest test-square-list
  (is (= '(1 4 9 16)
         (square-list-1 '(1 2 3 4))
         (square-list-2 '(1 2 3 4)))))

(deftest test-iterative-square-list
  (is (= '(1 4 9 16)
         (iterative-square-list '(1 2 3 4)))))

(deftest test-deep-reverse
  (is (= '((4 3) (2 1))
         (deep-reverse '((1 2) (3 4))))))

(deftest test-fringe
  (is (= '(1 2 3 4)
         (fringe '((1 2) (3 4))))))

(deftest test-square-tree
  (let [l '(1 (2 (3 4) 5 (6 7)))]
    (is (= '(1 (4 (9 16) 25 (36 49)))
           (square-tree-1 l)
           (square-tree-2 l)
           (square-tree-3 l)))))

(deftest test-subsets
  (is (= '((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())
         (subsets '(1 2 3)))))

(deftest test-map'-append'-length'
  (is (= '(1 4 9 16 25)
         (map' square '(1 2 3 4 5))))
  (is (= '(1 2 3 4)
         (append' '(1 2) '(3 4))))
  (is (= 5
         (length' '(1 2 3 4 5)))))

(deftest test-horner-eval
  (is (= 79 (horner-eval 2 (list 1 3 0 5 0 1)))))

(deftest test-count-leaves
  (is (= 4 (count-leaves '((1 2) (3 4))))))

(deftest test-accumulate-n
  (is (= '(22 26 30)
         (accumulate-n + 0 '((1  2  3)
                             (4  5  6)
                             (7  8  9)
                             (10 11 12))))))

(deftest test-matrix-operations
  (let [m '((1 2 3 4)
            (4 5 6 6)
            (6 7 8 9))]
    (is (= '(30 56 80)
           (matrix-*-vector m '(1 2 3 4))))
    (is (= '((1 4 6)
             (2 5 7)
             (3 6 8)
             (4 6 9))
           (transpose m)))
    (is (= '((30 56 80)
             (56 113 161)
             (80 161 230))
           (matrix-*-matrix m (transpose m))))))

(deftest test-reverse-fold
  (let [l '(1 2 3 4 5)]
    (is (= (reverse l)
           (reverse-fold-left l)
           (reverse-fold-right l)))))

(deftest test-prime-sum-pairs
  (is (every?
       (fn [[a b c]]
         (and (= c (+ a b))
              (prime? c)))
       (prime-sum-pairs 6))))

(deftest test-unique-triples-sum-equals-to
  (is (every?
       (fn [[a b c]] (= 10 (+ a b c)))
       (unique-triples-sum-equal-to 10))))

(deftest test-queens
  (is (= 92 (count (queens 8)))))

(deftest test-equal?
  (is (equal? '(a b c d) '(a b c d)))
  (is (not (equal? '(a b c d) '(a c b d)))))

(deftest test-deriv
  (is (= '(* 3 (** x 2))
         (deriv '(** x 3) 'x)))
  (is (= '(+ (* x y) (* y (+ x 3)))
         (deriv '(* x y (+ x 3)) 'x))))

(deftest test-dup-set
  (is (= '(2 2 1 2 2)
         (intersection-dup-set
          '(2 3 2 1 3 2 2) '(1 2))))
  (is (= '(2 3 2 1 3 2 2 3 4)
         (union-dup-set
          '(2 3 2 1 3 2 2) '(3 4)))))

(deftest test-sorted-set
  (is (= '(0 1 2 3)
         (adjoin-sorted-set 3 '(0 1 2))))
  (is (= '(1 2 3 4)
         (adjoin-sorted-set 3 '(1 2 4))))
  (is (= '(3 4 5 6)
         (adjoin-sorted-set 3 '(4 5 6))))
  (is (= '(1 2 3 4)
         (union-sorted-set '(1 3) '(2 4)))))

(deftest test-huffman-coding
  (let
    [sample-tree (make-code-tree
                  (make-leaf :A 4)
                  (make-code-tree
                   (make-leaf :B 2)
                   (make-code-tree
                    (make-leaf :D 1)
                    (make-leaf :C 1))))
     sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)]
    (is (= '(:A :D :A :B :B :C :A)
           (decode sample-message sample-tree)))
    (is (= sample-message
           (encode
            (decode sample-message sample-tree)
            sample-tree)))))

(deftest test-generate-huffman-tree
  (let
    [sample-tree (generate-huffman-tree
                  '((:A 4) (:B 2) (:D 1) (:C 1)))
     sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)]
    (is (= sample-message
           (encode
            (decode sample-message sample-tree)
            sample-tree)))))

(deftest test-deriv-data-directed
  (is (= '(+ (* x y) (* y (+ x 3)))
         (deriv-data-directed '(* (* x y) (+ x 3)) 'x)))
  (is (= '(* 3 (** x 2))
         (deriv-data-directed '(** x 3) 'x))))

(deftest test-make-from-mag-ang-msg-passing
  (let
    [a (Math/atan2 3 4)
     x (make-from-mag-ang-msg-passing 5 a)
     apply apply-generic-msg-passing]
    (is (= 5 (apply :magnitude x)))
    (is (≈ a (apply :angle     x)))
    (is (≈ 4 (apply :real-part x)))
    (is (≈ 3 (apply :imag-part x)))))

(deftest test-equ?
  (let [neq? (comp not equ?)]
    (is (equ? (make-scheme-number 1)
              (make-scheme-number 1)))
    (is (neq? (make-scheme-number 1)
              (make-scheme-number 2)))
    (is (equ? (make-rational 1 2)
              (make-rational 1 2)))
    (is (neq? (make-rational 1 2)
              (make-rational 1 3)))
    (is (equ? (make-complex-from-real-imag 1 2)
              (make-complex-from-real-imag 1 2)))
    (is (neq? (make-complex-from-real-imag 1 2)
              (make-complex-from-real-imag 1 3)))))

(deftest test-apply-generic-coercion
  (let [sum (apply-generic-coercion
             'add+
             (make-complex-from-real-imag 1 2)
             (make-scheme-number 2)
             (make-scheme-number 3))]
    (is (= 6 (real-part sum)))
    (is (= 2 (imag-part sum)))))
