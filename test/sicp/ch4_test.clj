(ns sicp.ch4_test
  (:require [clojure.test :refer :all]
            [sicp.ch4 :refer :all]))

;; (declare ^:dynamic *tmp*)
;; (deftest test-list-of-values-ltr
;;   (binding [*tmp* (atom 1)]
;;     (list-of-values-ltr '((reset! *tmp* 6)
;;                           (swap! *tmp* inc)))
;;     (is (= @*tmp* 7))))

(deftest test-pristine-eval
  (let [eval-1 (make-eval pristine-special-forms applicative-apply)
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
    (is (= (eval-1 '(begin (define foo 555)
                           (set! foo 666)
                           foo)
                   env)
           666))
    (is (= (eval-1 '(cond (false 1) (else 2 3)) env) 3))))

(deftest test-eval-and-or
  (let [eval-1 (make-eval special-forms-with-and-or applicative-apply)
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
  (let [eval-1 (make-eval special-forms-with-let applicative-apply)
        env (make-env pristine-primitives)]
    (is (= (eval-1 '(let ((a 1) (b 2) (c 3)) (+ a b c)) env) 6))))
