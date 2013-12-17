(ns sicp.ch4_test
  (:require [clojure.test :refer :all]
            [sicp.ch4 :refer :all]
            [backtick :refer :all]))

(deftest test-pristine-eval
  (let [eval-1 pristine-eval
        env (make-env pristine-primitives)]
    (is (= (eval-1 1 env) 1))
    (is (= (eval-1 "s" env) "s"))
    (is (= (eval-1 true env) true))
    (is (= (eval-1 false env) false))
    (is (= (eval-1 nil env) nil))
    (is (= (eval-1 '() env) '()))
    (is (= (eval-1 '(quote a) env)))
    (is (= (eval-1 '(if 1 'a 'b) env) 'a))
    (is (= (eval-1 '(if nil 'a 'b) env) 'b))
    (is (= (eval-1 '(begin 1 2 3 4) env) 4))
    (is (= (eval-1 '(begin (define foo 666) foo) env) 666))
    (is (= (eval-1 '(begin (define (add a b) (+ a b)) (add 3 4)) env) 7))
    (is (= (eval-1 '(begin (define foo 555) (set! foo 666) foo) env) 666))
    (is (= (eval-1 '(cond (false 1) (else 2 3)) env) 3))))

(deftest test-eval-and-or
  (let [eval-1 eval-with-and-or
        env (make-env pristine-primitives)]
    (is (= (eval-1 '(and 1) env) 1))
    (is (= (eval-1 '(and nil 1) env) nil))
    (is (= (eval-1 '(and 1 nil) env) nil))
    (is (= (eval-1 '(and 1 2 3) env) 3))
    (is (= (eval-1 '(or 1) env) 1))
    (is (= (eval-1 '(or nil 1) env) 1))
    (is (= (eval-1 '(or 1 nil) env) 1))
    (is (= (eval-1 '(or nil false) env) false))))

(deftest test-eval-let
  (let [eval-1 eval-with-let
        env (make-env pristine-primitives)]
    (is (= (eval-1 '(let ((a 1)
                          (b 2)
                          (c 3))
                      (+ a b c))
                   env)
           6))))

(deftest test-eval-let*
  (let [eval-1 eval-with-let*
        env (make-env pristine-primitives)]
    (is (= (eval-1 '(let* ((x 3)
                           (y (+ x 2))
                           (z (+ x y 5)))
                          (* x z))
                   env)
           39))))

(deftest test-named-let
  (let [eval-1 eval-with-named-let
        env (make-env pristine-primitives)]
    (is (= (eval-1 '(begin
                     (define (fib n)
                       (let fib-iter ((a 1)
                                      (b 0)
                                      (count n))
                            (if (= count 0)
                              b
                              (fib-iter (+ a b) a (- count 1)))))
                     (fib 4))
                   env)
           3))))

(deftest test-*unassigned*
  (let [eval-1 pristine-eval
        env (make-unassignable-env pristine-primitives)]
    (is (thrown-with-msg?
         Exception
         #"Unassigned variable: foo"
         (eval-1 '(begin
                   (define foo '*unassigned*)
                   foo)
                 env)))))

(deftest test-letrec
  (let [eval-1 eval-with-leterec
        env (make-env pristine-primitives)]
    (is (= (eval-1 '(letrec ((fact
                              (lambda (n)
                                      (if (= n 1)
                                        1
                                        (* n (fact (- n 1)))))))
                            (fact 10))
                   env)
           3628800))))

(deftest test-recursive-even?
  (is (true?  (recursive-even? 0)))
  (is (true?  (recursive-even? 2)))
  (is (false? (recursive-even? 1))))

(deftest test-unless
  (let [eval-1 eval-with-unless
        env (make-env pristine-primitives)]
    (is (= (eval-1 '(unless true  'a 'b) env) 'b))
    (is (= (eval-1 '(unless false 'a 'b) env) 'a))))

(deftest test-lazy-eval
  (let [eval-1 pristine-eval-lazy
        env (make-env pristine-primitives-non-strict)]
    (is (= (eval-1
            '(begin
              (define (try a b)
                (if (= a 0) 1 b))
              (try 0 (/ 1 0)))
            env)
           1))))

(deftest test-mixed-eval
  (let [eval-1 pristine-eval-mixed
        env (make-env pristine-primitives-non-strict)]
    (is (= (eval-1
            '(begin
              (define count 0)
              (define (id x)
                (set! count (+ count 1))
                x)
              (define (square (x lazy)) (* x x))
              (square (id 20))
              count)
            env)
           2))
    (is (= (eval-1
            '(begin
              (define count 0)
              (define (id x)
                (set! count (+ count 1))
                x)
              (define (square (x lazy-memo)) (* x x))
              (square (id 20))
              count)
            env)
           1))))

(deftest test-amb-eval
  (let [env  (make-env pristine-primitives)
        fail (fn [value] (is (= 1 0)))]
    (amb-eval '(begin
                (define (require p) (if (not p) (amb)))
                (define (an-element-of items)
                  (require (not (null? items)))
                  (amb (car items) (an-element-of (cdr items))))
                (define (sum-pair list1 list2 sum)
                  (let ((a (an-element-of list1))
                        (b (an-element-of list2)))
                    (require (= sum (+ a b)))
                    (list a b)))
                (sum-pair '(1 3 5 8) '(20 35 110) 23))
              env
              (fn [value fail] (is (= value '(3 20))))
              fail)))

(deftest test-an-integer-between
  (let [env (make-env pristine-primitives)]
    (amb-eval (template (begin
                         (define (require p) (if (not p) (amb)))
                         ~an-integer-between
                         (define (a-pythagorean-triple-between low high)
                           (let ((i (an-integer-between low high)))
                             (let ((j (an-integer-between i high)))
                               (let ((k (an-integer-between j high)))
                                 (require (= (+ (* i i) (* j j)) (* k k)))
                                 (list i j k)))))
                         (a-pythagorean-triple-between 1 5)))
              env
              (fn [value fail] (is false))
              #(is true))))
