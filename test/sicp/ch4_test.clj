(ns sicp.ch4_test
  (:refer-clojure :exclude [==])
  (:use [clojure.test]
        [sicp.utils :only [quasiquote]]
        [clojure.core.logic :exclude [is]]
        [clojure.core.logic.pldb]
        [sicp.ch4]))

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
        fail #(is (= 1 0))]
    (amb-eval
     (quasiquote
      (begin
       ~@amb-utils
       (define (prime-sum-pair list1 list2)
         (let ((a (an-element-of list1))
               (b (an-element-of list2)))
           (require (prime? (+ a b)))
           (list a b)))
       (prime-sum-pair '(1 3 5 8) '(20 35 110))))
     env
     (fn [value fail] (is (= value '(3 20))))
     fail)))

(deftest test-an-integer-between
  (let [env (make-env pristine-primitives)]
    (amb-eval
     (quasiquote
      (begin
       ~@amb-utils
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

(deftest test-amb-pythagorean-triples
  (testing "exercise 4.36"
    (is (= (take 5 amb-pythagorean-triples)
           '([3 4 5] [6 8 10] [5 12 13] [9 12 15] [8 15 17])))))

(deftest test-multiple-dwelling-1
  (testing "exercise 4.38"
    (is (= (count multiple-dwelling-1) 15))))

(deftest test-multiple-dwelling-ordinary
  (testing "exercise 4.41"
    (is (= (multiple-dwelling-ordinary)
           multiple-dwelling))))

(deftest test-liar-puzzle
  (testing "exercise 4.42"
    (is (= liar-puzzle
           '([:betty 3 :ethel 5 :joan 2 :kitty 1 :mary 4])))))

(deftest test-yacht-puzzle
  (testing "exercise 4.43"
    (is (= yacht-puzzle
           '(:downing)))
    (is (= (count yacht-puzzle-1) 2))))

(deftest test-amb-solve-8-queens
  (testing "exercise 4.44"
    (is (= (count (amb-solve-8-queens))
           92))))

(deftest test-amb-permanent-set!
  (testing "exercise 4.51"
    (let [env  (make-env pristine-primitives)
          fail #(is (= 1 0))]
      (permanent-set-amb-eval
       (quasiquote
        (begin
         ~@amb-utils
         (define count 0)
         (let ((x (an-element-of '(a b c)))
               (y (an-element-of '(a b c))))
           (permanent-set! count (+ count 1))
           (require (not (= x y)))
           (list x y count))))
       env
       (fn [value fail] (is (= value '(a b 2))))
       fail))))

(deftest test-if-fail-amb-eval
  (testing "exercise 4.52"
    (let [env  (make-env pristine-primitives)
          fail #(is (= 1 0))]
      (if-fail-amb-eval
       (quasiquote
        (begin
         ~@amb-utils
         (if-fail (let ((x (an-element-of '(1 3 5))))
                    (require (even? x))
                    x)
                  'all-odd)))
       env
       (fn [value fail] (is (= value 'all-odd)))
       fail)
      (if-fail-amb-eval
       (quasiquote
        (begin
         ~@amb-utils
         (if-fail (let ((x (an-element-of '(1 3 5 8))))
                    (require (even? x))
                    x)
                  'all-odd)))
       env
       (fn [value fail] (is (= value 8)))
       fail))))

(deftest test-amb-eval-for-4-53
  (testing "exercise 4.53"
    (let [env  (make-env pristine-primitives)
          fail #(is (= 1 0))]
      (amb-eval-for-4-53
       (quasiquote
        (begin
         ~@amb-utils
         (define (prime-sum-pair list1 list2)
           (let ((a (an-element-of list1))
                 (b (an-element-of list2)))
             (require (prime? (+ a b)))
             (list a b)))
         (let ((pairs '()))
           (if-fail (let ((p (prime-sum-pair '(1 3 5 8)
                                             '(20 35 110))))
                      (permanent-set! pairs (cons p pairs))
                      (amb))
                    pairs))))
       env
       (fn [value fail] (is (= value '((8 35)
                                       (3 110)
                                       (3 20)))))
       fail))))

(deftest test-special-require-amb-eval
  (testing "exercise 4.54"
    (let [env  (make-env pristine-primitives)
          fail #(is (= 1 0))]
      (special-require-amb-eval
       '(begin
         (define (an-element-of items)
           (require (not (null? items)))
           (amb (car items) (an-element-of (cdr items))))
         (define (prime-sum-pair list1 list2)
           (let ((a (an-element-of list1))
                 (b (an-element-of list2)))
             (require (prime? (+ a b)))
             (list a b)))
         (prime-sum-pair '(1 3 5 8) '(20 35 110)))
       env
       (fn [value fail] (is (= value '(3 20))))
       fail))))

(deftest test-simple-queries
  (testing "exercise 4.55"
    (is (= (into #{} supervised-by-ben-bitdiddle)
           #{[:Fect :Cy :D] [:Hacker :Alyssa :P] [:Tweakit :Lem :E]}))
    (is (= (into #{} people-in-accounting-division)
           #{[[:Cratchet :Robert] [:accounting :scrivener]]
             [[:Scrooge :Eben] [:accounting :chief :accountant]]}))
    (is (= (into #{} people-in-slumerville)
           #{[[:Reasoner :Louis] [:Slumerville [:Pine :Tree :Road] 80]]
             [[:Bitdiddle :Ben] [:Slumerville [:Ridge :Road] 10]]
             [[:Aull :DeWitt] [:Slumerville [:Onion :Square] 5]]}))))

(deftest test-compound-queries
  (testing "exercise 4.56"
    (is (= (into #{} supervised-by-ben-bitdiddle-with-address)
           #{[[:Fect :Cy :D] [:Cambridge [:Ames :Street] 3]]
             [[:Hacker :Alyssa :P] [:Cambridge [:Mass :Ave] 78]]
             [[:Tweakit :Lem :E] [:Boston [:Bay :State :Road] 22]]}))
    (is (= (into #{} salary-less-than-ben-bitdiddle-s)
           #{[[:Tweakit :Lem :E] 25000 60000]
             [[:Cratchet :Robert] 18000 60000]
             [[:Fect :Cy :D] 35000 60000]
             [[:Reasoner :Louis] 30000 60000]
             [[:Aull :DeWitt] 25000 60000]
             [[:Hacker :Alyssa :P] 40000 60000]}))
    (is (= (into #{} supervised-not-in-computer-division)
           #{[[:Scrooge :Eben]
              [:Warbucks :Oliver]
              [:administration :big :wheel]]
             [[:Aull :DeWitt]
              [:Warbucks :Oliver]
              [:administration :big :wheel]]
             [[:Bitdiddle :Ben]
              [:Warbucks :Oliver]
              [:administration :big :wheel]]
             [[:Cratchet :Robert]
              [:Scrooge :Eben]
              [:accounting :chief :accountant]]}))))

(deftest test-can-replace-rule
  (testing "exercise 4.57"
    (is (= (into #{} can-replace-cy-d-fect)
           #{[:Bitdiddle :Ben]
             [:Hacker :Alyssa :P]}))
    (is (= (into #{} can-replace-who-is-being-paid-more)
           #{[[:Fect :Cy :D] 35000 40000]
             [[:Aull :DeWitt] 25000 150000]}))))

(deftest test-big-shot
  (testing "exercise 4.58"
    (is (= (into #{}
                 (run-db* facts [q]
                          (fresh [a b]
                                 (big-shot a b)
                                 (== q [a b]))))
           #{[[:Bitdiddle :Ben] :computer]
             [[:Scrooge :Eben] :accounting]}))))

(deftest test-meetings
  (testing "exercise 4.59"
    (is (= (into #{} meetings-at-friday)
           #{[:administration [:Friday :1pm]]}))
    (is (= (into #{} alyssa-meetings-at-wednesday)
           #{[:whole-company [:Wednesday :4pm]]
             [:computer [:Wednesday :3pm]]}))))

(deftest test-asymmetric-lives-near
  (testing "exercise 4.60"
    (is (= (into #{} #_microshaft-neighborhood
                 (run-db* facts [q]
                          (fresh [a b]
                                 (asymmetric-lives-near a b)
                                 (== q [a b]))))
           #{[[:Fect :Cy :D] [:Hacker :Alyssa :P]]
             [[:Bitdiddle :Ben] [:Reasoner :Louis]]
             [[:Aull :DeWitt] [:Reasoner :Louis]]
             [[:Aull :DeWitt] [:Bitdiddle :Ben]]}))))

(deftest test-next-to
  (testing "exercise 4.61"
    (is (= (into #{}
                 (run* [q]
                       (fresh [x y]
                              (next-to x y [1 [2 3] 4])
                              (== q [x y]))))
           #{[1 [2 3]] [[2 3] 4]}))
    (is (= (into #{}
                 (run* [q]
                       (next-to q 1 [2 1 3 1])))
           #{2 3}))))

(deftest test-last-pair
  (testing "exercise 4.62"
    (is (= (->> (run* [q] (last-pair [3] q))
                (map #(into [] %))
                (into #{}))
           #{[3]}))
    (is (= (->> (run* [q] (last-pair [1 2 3] q))
                (map #(into [] %))
                (into #{}))
           #{[3]}))
    (is (= (->> (run* [q] (last-pair [2 q] [3]))
                (into #{}))
           #{3}))))

(deftest test-generations-of-adam
  (testing "exercise 4.63"
    (is (= (into #{}
                 (run-db* generations-of-adam [q]
                          (grandson-of :Cain q)))
           #{:Irad}))
    (is (= (into #{}
                 (run-db* generations-of-adam [q]
                          (son-of :Lamech q)))
           #{:Jubal :Jabal}))
    (is (= (into #{}
                 (run-db* generations-of-adam [q]
                          (grandson-of :Methushael q)))
           #{:Jubal :Jabal}))))

(deftest test-unidirectional-reverseo
  (testing "exercise 4.68"
    (is (= (run* [q] (reverseo [] q)) '([])))
    (is (= (run* [q] (reverseo [42] q)) '((42))))
    (is (= (run* [q] (reverseo [1 2 3] q)) '((3 2 1))))))

(deftest test-qeval
  (testing "query system implementation"
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (job ?x (computer programmer))))))
           #{'(job (Hacker Alyssa P) (computer programmer))
             '(job (Fect Cy D) (computer programmer))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (job ?x (computer ?type))))))
           #{'(job (Bitdiddle Ben) (computer wizard))
             '(job (Hacker Alyssa P) (computer programmer))
             '(job (Fect Cy D) (computer programmer))
             '(job (Tweakit Lem E) (computer technician))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (job ?x (computer . ?type))))))
           #{'(job (Bitdiddle Ben) (computer wizard))
             '(job (Hacker Alyssa P) (computer programmer))
             '(job (Fect Cy D) (computer programmer))
             '(job (Tweakit Lem E) (computer technician))
             '(job (Reasoner Louis) (computer programmer trainee))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (and (job ?person (computer programmer))
                         (address ?person ?where))))))
           #{'(and (job (Hacker Alyssa P) (computer programmer))
                   (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
             '(and (job (Fect Cy D) (computer programmer))
                   (address (Fect Cy D) (Cambridge (Ames Street) 3)))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (or (supervisor ?x (Bitdiddle Ben))
                        (supervisor ?x (Hacker Alyssa P)))))))
           #{'(or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
                  (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
             '(or (supervisor (Fect Cy D) (Bitdiddle Ben))
                  (supervisor (Fect Cy D) (Hacker Alyssa P)))
             '(or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
                  (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
             '(or (supervisor (Reasoner Louis) (Bitdiddle Ben))
                  (supervisor (Reasoner Louis) (Hacker Alyssa P)))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (and (supervisor ?x (Bitdiddle Ben))
                         (not (job ?x (computer programmer))))))))
           #{'(and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
                   (not (job (Tweakit Lem E) (computer programmer))))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (and (salary ?person ?amount)
                         (lisp-value > ?amount 30000))))))
           #{'(and (salary (Scrooge Eben) 75000)
                   (lisp-value > 75000 30000))
             '(and (salary (Warbucks Oliver) 150000)
                   (lisp-value > 150000 30000))
             '(and (salary (Fect Cy D) 35000)
                   (lisp-value > 35000 30000))
             '(and (salary (Hacker Alyssa P) 40000)
                   (lisp-value > 40000 30000))
             '(and (salary (Bitdiddle Ben) 60000)
                   (lisp-value > 60000 30000))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (assert!
                     (rule (lives-near ?person-1 ?person-2)
                           (and (address ?person-1 (?town . ?rest-1))
                                (address ?person-2 (?town . ?rest-2))
                                (not (same ?person-1 ?person-2)))))
                    (assert! (rule (same ?x ?x)))
                    (lives-near ?x (Bitdiddle Ben))))))
           #{'(lives-near (Reasoner Louis) (Bitdiddle Ben))
             '(lives-near (Aull DeWitt) (Bitdiddle Ben))}))
    (is (= (into #{}
                 (qeval
                  (quasiquote
                   (~@facts-1
                    (assert!
                     (rule
                      (outranked-by ?staff-person ?boss)
                      (or (supervisor ?staff-person ?boss)
                          (and (supervisor ?staff-person ?middle-manager)
                               (outranked-by ?middle-manager ?boss)))))
                    (outranked-by (Hacker Alyssa P) ?who)))))
           #{'(outranked-by (Hacker Alyssa P) (Bitdiddle Ben))
             '(outranked-by (Hacker Alyssa P) (Warbucks Oliver))}))))

(deftest test-simple-stream-flatmap
  (testing "exercise 4.74"
    (is (= (simple-stream-flatmap identity [[1] [] [2] [] [3]])
           [1 2 3]))))

;; The system's behavior will not change. In fact, the program can be
;; made simpler using only `stream-filter`.

;; Exercise 4.75

(deftest test-qeval-with-unique
  (testing "exercise 4.75"
    (is (= (into #{}
                 (qeval-with-unique
                  (quasiquote (~@facts-1
                               (unique (job ?x (computer wizard)))))))
           #{'(unique (job (Bitdiddle Ben) (computer wizard)))}))
    (is (= (into #{}
                 (qeval-with-unique
                  (quasiquote (~@facts-1
                               (and (supervisor ?x ?s)
                                    (unique (supervisor ?anyone ?s)))))))
           #{'(and (supervisor (Reasoner Louis) (Hacker Alyssa P))
                   (unique (supervisor (Reasoner Louis)
                                       (Hacker Alyssa P))))
             '(and (supervisor (Cratchet Robert) (Scrooge Eben))
                   (unique (supervisor (Cratchet Robert)
                                       (Scrooge Eben))))}))))

(deftest test-qeval-with-alt-conjoin
  (testing "exercise 4.76"
    (is (= (into #{}
                 (qeval-with-alt-conjoin
                  (quasiquote
                   (~@facts-1
                    (and (job ?person (computer programmer))
                         (address ?person ?where))))))
           #{'(and (job (Hacker Alyssa P) (computer programmer))
                   (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
             '(and (job (Fect Cy D) (computer programmer))
                   (address (Fect Cy D) (Cambridge (Ames Street) 3)))}))))

(deftest test-qeval-with-filter-fix
  (testing "exercise 4.77"
    (is (= (into #{}
                 (qeval-with-filter-fix
                  (quasiquote
                   (~@facts-1
                    (and (not (job ?x (computer programmer)))
                         (supervisor ?x (Bitdiddle Ben)))))))
           #{'(and (not (job (Tweakit Lem E) (computer programmer)))
                   (supervisor (Tweakit Lem E) (Bitdiddle Ben)))}))
    (is (= (into #{}
                 (qeval-with-filter-fix
                  (quasiquote
                   (~@facts-1
                    (and (lisp-value > ?amount 30000)
                         (salary ?person ?amount))))))
           #{'(and (lisp-value > 75000 30000)
                   (salary (Scrooge Eben) 75000))
             '(and (lisp-value > 150000 30000)
                   (salary (Warbucks Oliver) 150000))
             '(and (lisp-value > 35000 30000)
                   (salary (Fect Cy D) 35000))
             '(and (lisp-value > 40000 30000)
                   (salary (Hacker Alyssa P) 40000))
             '(and (lisp-value > 60000 30000)
                   (salary (Bitdiddle Ben) 60000))}))))
