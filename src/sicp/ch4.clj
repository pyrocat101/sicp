;;;; Chapter 4: Metalinguistic Abstraction
;;;; =====================================

;;; Namespace and dependencies

(ns sicp.ch4
  (:use [backtick]))

;;; a basic evaluator

(defn eval-seq [eval]
  (fn [coll env]
    (if (empty? coll)
      nil
      (loop [exps coll]
        (if (empty? (rest exps))
          (eval (first exps) env)
          (do
            (eval (first exps) env)
            (recur (rest exps))))))))

;; environment

(defprotocol Environmental
  "Something that is environment-friendly"
  (env-extend  [this bindings])
  (env-lookup  [this var])
  (env-set!    [this var value])
  (env-define! [this var value]))

(deftype Env [bindings enclose])

(def base-env-behavior
  {:env-extend (fn [this bindings]
                 (Env. (atom bindings) this))
   :env-lookup (fn [this var]
                 (loop [env this]
                   (let [bindings @(.bindings env)
                         enclose   (.enclose  env)]
                     (if (contains? bindings var)
                       (get bindings var)
                       (if-not enclose
                         (throw (Exception.
                                 (print-str "Unbound variable:" var)))
                         (recur enclose))))))
   :env-set! (fn [this var value]
               (loop [env this]
                 (let [bindings (.bindings env)
                       enclose  (.enclose  env)]
                   (if (contains? @bindings var)
                     (do
                       (swap! bindings assoc var value)
                       var)
                     (if-not enclose
                       (throw (Exception.
                               (print-str "Unbound variable:" var)))
                       (recur enclose))))))
   :env-define! (fn [this var value]
                  (swap! (.bindings this) assoc var value)
                  var)})

(extend Env
  Environmental
  base-env-behavior)

(defn make-env
  ([bindings]
     (make-env bindings nil))
  ([bindings enclose]
     (Env. (atom bindings) enclose)))

;; procedure and primitive

(defprotocol Appliable
  "Something that can be applied"
  (my-apply [this args env eval]))

(deftype Procedure [params body env]
  Appliable
  (my-apply [this args env eval]
    (let [env (env-extend (.env this)
                          (zipmap (.params this) args))]
      ((eval-seq eval) (.body this) env))))

(deftype Primitive [fn]
  Appliable
  (my-apply [this args env eval]
    (apply (.fn this) args)))

(defn applicative-apply
  [exp env eval]
  (let [[proc & args] (map #(eval % env) exp)]
    (my-apply proc args env eval)))

;; primitive expressions

(defn self-eval?
  [exp]
  (or (number? exp)
      (string? exp)
      (true? exp)
      (false? exp)
      (nil? exp)
      (= exp '())))

(defn variable?
  [exp]
  (symbol? exp))

;; make evaluator

(defn make-eval
  [special-forms apply-fn]
  (fn ! [exp env]
    (cond (self-eval? exp) exp
          (variable? exp)  (env-lookup env exp)
          ;; special forms
          :else
          (let [[op & rest] exp
                op (keyword op)]
            (if (contains? special-forms op)
              ;; special forms
              ((get special-forms op) rest env !)
              ;; application
              (apply-fn exp env !))))))

;; basic special forms

(defn eval-quote [[x] _ _] x)

(defn eval-assign
  [[v o] env eval]
  (env-set! env v (eval o env))
  'ok)

(defn desugar-define
  [[name & params] & body]
  (template (define ~name (lambda ~params ~@body))))

(defn eval-define
  [exp env eval]
  (if (symbol? (first exp))
    (let [[v o] exp]
      (env-define! env v (eval o env)))
    (eval (apply desugar-define exp) env))
  'ok)

(defn eval-if
  [[pred conseq alt] env eval]
  (if (eval pred env)
    (eval conseq env)
    (eval alt    env)))

(defn eval-lambda
  [[params & body] env _]
  (Procedure. params body env))

(defn eval-begin
  [exp env eval]
  ((eval-seq eval) exp env))

(defn seq->exp
  ([] nil)
  ([coll]
     (cond (empty? coll) coll
           (empty? (rest coll)) (first coll)
           :else (template (begin ~@coll)))))

(defn cond->if
  ([] false)
  ([[[pred & act] & next :as clauses]]
     (if (= pred 'else)
       (if (empty? next)
         (seq->exp act)
         (throw (Exception.
                 (print-str "ELSE clause isn't last:" clauses))))
       (list 'if pred (seq->exp act) (cond->if next)))))

(defn eval-cond
  [exp env eval]
  (eval (cond->if exp) env))

;; basic primitives

(def pristine-primitives-map
  {'car   first
   'cdr   rest
   'cons  cons
   'null? empty?
   'not   not
   'list  list
   '+     +
   '-     -
   '*     *
   '/     /
   '=     =
   '<     <
   '>     >
   '<=    <=
   '>=    >=} )

(def pristine-primitives
  (let [m pristine-primitives-map]
    (into {} (for [[k v] m] [k (Primitive. v)]))))

;; basic special form

(def pristine-special-forms
  {:quote   eval-quote
   :set!    eval-assign
   :define  eval-define
   :if      eval-if
   :lambda  eval-lambda
   :begin   eval-begin
   :cond    eval-cond})

;; pristine evaluator

(def pristine-eval
  (make-eval pristine-special-forms applicative-apply))

;; Exercise 4.1

(defn list-of-values-ltr [exps]
  (if (empty? exps)
    '()
    (#(cons % (list-of-values-ltr (rest exps)))
     (eval (first exps)))))

;; Exercise 4.2

;; Louis is an idiot.

;; Exercise 4.4

(defn and->if
  ([] true)
  ([x] x)
  ([x & next]
     (list 'if x (apply and->if next) x)))

(defn eval-and
  [exp env eval]
  (eval (apply and->if exp) env))

(defn or->if
  ([] nil)
  ([x] x)
  ([x & next]
     (list 'if x x (apply or->if next))))

(defn eval-or
  [exp env eval]
  (eval (apply or->if exp) env))

(def special-forms-with-and-or
  (assoc pristine-special-forms :and eval-and :or eval-or))

(def eval-with-and-or
  (make-eval special-forms-with-and-or applicative-apply))

;; Exercise 4.6

(defn let->combination
  [bindings & body]
  (let [names  (map first bindings)
        values (map second bindings)]
    (template ((lambda ~names ~@body) ~@values))))

(defn eval-let
  [exp env eval]
  (eval (apply let->combination exp) env))

(def special-forms-with-let
  (assoc pristine-special-forms :let eval-let))

(def eval-with-let
  (make-eval special-forms-with-let applicative-apply))

;; Exercise 4.7

(defn let*->nested-lets
  [bindings & body]
  (letfn [(iter [names values]
            (if (or (empty? (rest names))
                    (empty? (rest values)))
              (template (let ((~(first names) ~(first values))) ~@body))
              (template (let ((~(first names) ~(first values)))
                          ~(iter (rest names) (rest values))))))]
    (iter (map first bindings) (map second bindings))))

(defn eval-let*
  [exp env eval]
  (eval (apply let*->nested-lets exp) env))

(def special-forms-with-let*
  (assoc special-forms-with-let :let* eval-let*))

(def eval-with-let*
  (make-eval special-forms-with-let* applicative-apply))

;;; Exercise 4.8

(defn named-let->combination
  [& args]
  (if (symbol? (first args))
    (let [[name bindings & body] args]
      (template (((lambda ()
                           (define (~name ~@(map first bindings)) ~@body)
                           ~name))
                 ~@(map second bindings))))
    (apply let->combination args)))

(defn eval-named-let
  [exp env eval]
  (eval (apply named-let->combination exp) env))

(def special-forms-with-named-let
  (assoc pristine-special-forms :let eval-named-let))

(def eval-with-named-let
  (make-eval special-forms-with-named-let applicative-apply))

;; (def eval-1 (make-eval pristine-special-forms applicative-apply))
;; (def env (make-env pristine-primitives))

;; Exercise 4.14

;; Because the procedure in our evaluator is different from the
;; underlying language. Therefore, it cannot be called by
;; system version of procedures.

;; Exercise 4.15

;; It is a paradox.
;; If `(try try)` returns, it means `try` halts on `try`.
;; But it also means that `halts?` returns false, which means
;; `try` is not halt-able.
;; And vice versa.

;; Exercise 4.16

(def env-with-unassigned-behavior
  (merge base-env-behavior
         {:env-lookup
          (fn [this var]
            (loop [env this]
              (let [bindings @(.bindings env)
                    enclose   (.enclose  env)]
                (if (contains? bindings var)
                  (let [val (get bindings var)]
                    (if (= val '*unassigned*)
                      (throw (Exception.
                              (print-str "Unassigned variable:" var)))
                      val))
                  (if-not enclose
                    (throw (Exception.
                            (print-str "Unbound variable:" var)))
                    (recur enclose))))))}))

(deftype UnassignableEnv [bindings enclose])
(extend UnassignableEnv
  Environmental
  env-with-unassigned-behavior)

(defn make-unassignable-env
  ([bindings]
     (make-unassignable-env bindings nil))
  ([bindings enclose]
     (UnassignableEnv. (atom bindings) enclose)))

;; Exercise 4.18

;; It doesn't work. `u` and `v` are both assigned when `<e1>` and `<e2>`
;; are evaluated in the `let` form.

;; Exercise 4.19

(defn letrec->let
  [bindings & body]
  (let [names  (map first  bindings)
        values (map second bindings)]
    (template (let ~(map #(list % ''*unassigned*) names)
                ~@((fn ! [bindings]
                     (if (empty? bindings)
                       body
                       (let [[[k v] & next] bindings]
                         (template ((set! ~k ~v) ~@(! next)))
                         #_(cons (list 'set! k v)
                               (! next)))))
                   bindings)))))

(defn eval-letrec
  [exp env eval]
  (eval (apply letrec->let exp) env))

(def special-forms-with-letrec
  (assoc special-forms-with-let :letrec eval-letrec))

(def eval-with-leterec
  (make-eval special-forms-with-letrec applicative-apply))

;; Exercise 4.21

(defn recursive-even?
  [x]
  ((fn [even? odd?]
     (even? even? odd? x))
   (fn [ev? od? n]
     (if (zero? n) true (od? ev? od? (dec n))))
   (fn [ev? od? n]
     (if (zero? n) false (ev? ev? od? (dec n))))))

;; Exercise 4.25

;; the expression `(* n (factorial (- n 1)))` will be evaluated endlessly.

;; Exercise 4.26

(defn unless->if
  [pred conseq alt]
  (list 'if pred alt conseq))

(defn eval-unless
  [exp env eval]
  (eval (apply unless->if exp) env))

(def special-forms-with-unless
  (assoc pristine-special-forms :unless eval-unless))

(def eval-with-unless
  (make-eval special-forms-with-unless applicative-apply))

;; Exercise 4.27

;; (define w (id (id 10)))
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval output:
;; 1
;; ;;; L-Eval input:
;; w
;; ;;; L-Eval value:
;; 10
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 2

;; Defining `w` causes outer `id` to be evaluated.
;; Printing the value of `w` causes inner `id` to be evaluated.

;; Exercise 4.28

;; cannot apply a thunk on the arguments:
;; ((lambda (x) x) 42)

;; Exercise 4.29

;; With memorization, `(id 10)` is called once.
;; Without memorization, `(id 10)` is called twice.

;; Exercise 4.30

;; a. Ben is right because `display` is a primitive procedure.
;; b. original: (p1 1) => (1 2), (p2 1) => 1
;;    Cy's version: (p1 1) => (1 2), (p2 1) => (1 2)
;; c. `actual-value` acts the same as `eval` on primitive procedures.

;; Exercise 4.32

(declare force-it actual-value)

(defprotocol Thinkable
  (force-thunk [_ eval]))

(deftype SimpleThunk [exp env])
(deftype CachedThunk [exp env])

(defn- force-simple-thunk [this eval]
  (actual-value (.exp this) (.env this) eval))

(extend SimpleThunk
  Thinkable
  {:force-thunk force-simple-thunk})

(extend CachedThunk
  Thinkable
  {:force-thunk (memoize force-simple-thunk)})

(defn actual-value
  [exp env eval]
  (force-it (eval exp env) eval))

(defn force-it
  [obj eval]
  (if (satisfies? Thinkable obj)
    (force-thunk obj eval)
    obj))

(deftype NonStrictPrimitive [fn]
  Appliable
  (my-apply [this args env eval]
    (apply (.fn this) (map #(actual-value % env eval) args))))

(deftype NonStrictProcedure [params body env]
  Appliable
  (my-apply [this args env eval]
    (let [env (env-extend (.env this)
                          (zipmap (.params this)
                                  (map #(CachedThunk. % env) args)))]
      ((eval-seq eval) (.body this) env))))

(defn normative-apply
  [exp env eval]
  (let [proc (actual-value (first exp) env eval)
        args (rest exp)]
    (my-apply proc args env eval)))

(defn eval-lambda-non-strict
  [[params & body] env _]
  (NonStrictProcedure. params body env))

(defn eval-if-lazy
  [[pred conseq alt] env eval]
  (if (actual-value pred env eval)
    (eval conseq env)
    (eval alt    env)))

(def pristine-special-forms-lazy
  (assoc pristine-special-forms
    :lambda eval-lambda-non-strict
    :if     eval-if-lazy))

(def pristine-primitives-non-strict
  (let [m pristine-primitives-map]
    (into {} (for [[k v] m] [k (NonStrictPrimitive. v)]))))

(def pristine-eval-lazy
  (make-eval pristine-special-forms-lazy normative-apply))

(defn maybe-delay
  [arg param env eval]
  (cond (symbol? param) (actual-value arg env eval)
        (= 'lazy (second param)) (SimpleThunk. arg env)
        (= 'lazy-memo (second param)) (CachedThunk. arg env)))

(defn get-param-symbol
  [param]
  (if (symbol? param)
    param
    (first param)))

(deftype MixedProcedure [params body env]
  Appliable
  (my-apply [this args env eval]
    (let [params (.params this)
          env (env-extend (.env this)
                          (zipmap (map get-param-symbol params)
                                  (map #(maybe-delay %1 %2 env eval)
                                       args
                                       params)))]
      ((eval-seq eval) (.body this) env))))

(defn eval-lambda-mixed
  [[params & body] env _]
  (MixedProcedure. params body env))

(def pristine-special-forms-mixed
  (assoc pristine-special-forms
    :lambda eval-lambda-mixed))

(def pristine-eval-mixed
  (make-eval pristine-special-forms-mixed normative-apply))

;; amb evaluator

(declare amb-analyze-apply)

(defn make-amb-analyze
  [analyzors]
  (fn ! [exp]
    (cond (self-eval? exp)
          (fn [env succeed fail]
            (succeed exp fail))
          (variable? exp)
          (fn [env succeed fail]
            (succeed (env-lookup env exp) fail))
          :else
          (let [[op & rest] exp
                op (keyword op)]
            (if (contains? analyzors op)
              ;; special form analyzors
              ((get analyzors op) rest !)
              ;; application
              (amb-analyze-apply exp !))))))

(defn amb-analyze-seq
  [analyze]
  (fn [coll]
    (let [sequentially
          (fn [a b]
            (fn [env succeed fail]
              (a env
                 (fn [a-val fail]
                   (b env succeed fail))
                 fail)))
          procs (map analyze coll)]
      (if (empty? procs)
        (throw (Exception. "Empty sequence -- ANALYZE"))
        (loop [head (first procs)
               next (rest  procs)]
          (if (empty? next)
            head
            (recur (sequentially head (first next))
                   (rest next))))))))

;; basic amb special form analyzors

(defn amb-analyze-quote [[x] _]
  (fn [env succeed fail]
    (succeed x fail)))

(defn amb-analyze-lambda
  [[params & body] analyze]
  (fn [env succeed fail]
    (succeed (Procedure. params
                         ((amb-analyze-seq analyze) body)
                         env)
             fail)))

(defn amb-analyze-if
  [[pred conseq alt] analyze]
  (let [pred   (analyze pred)
        conseq (analyze conseq)
        alt    (analyze alt)]
    (fn [env succeed fail]
      (pred env
            ;; success continuation
            (fn [pred-value fail]
              (if pred-value
                (conseq env succeed fail)
                (alt    env succeed fail)))
            ;; failure continuation
            fail))))

(defn amb-analyze-define
  [exp analyze]
  (if (symbol? (first exp))
    (let [[v o] exp
          o (analyze o)]
      (fn [env succeed fail]
        (o env
           (fn [val fail]
             (env-define! env v val)
             (succeed 'ok fail))
           fail)))
    (analyze (apply desugar-define exp))))

(defn amb-analyze-assign
  [[v o] analyze]
  (let [o (analyze o)]
    (fn [env succeed fail]
      (o env
         (fn [val fail]
           (let [old-val (env-lookup env v)]
             (env-set! env v val)
             (succeed 'ok
                      ;; undo assignment
                      (fn []
                        (env-set! env v old-val)
                        (fail)))))
         fail))))

(defprotocol AmbAppliable
  (amb-apply [this args succeed fail]))

(extend-protocol AmbAppliable
  Procedure
  (amb-apply [this args succeed fail]
    (let [env (env-extend (.env this)
                          (zipmap (.params this) args))]
      ((.body this) env succeed fail)))
  Primitive
  (amb-apply [this args succeed fail]
    (succeed (my-apply this args nil nil) fail)))

(defn amb-get-args
  [aprocs env succeed fail]
  (if (empty? aprocs)
    (succeed '() fail)
    ((first aprocs)
     env
     (fn [arg fail]
       (amb-get-args (rest aprocs)
                     env
                     (fn [args fail]
                       (succeed (cons arg args)
                                fail))
                     fail))
     fail)))

(defn amb-analyze-apply
  [exp analyze]
  (let [[proc & args] (map analyze exp)]
    (fn [env succeed fail]
      (proc env
            (fn [proc fail]
              (amb-get-args args
                            env
                            (fn [args fail]
                              (amb-apply proc
                                         args
                                         succeed
                                         fail))
                            fail))
            fail))))

(defn amb-analyze-amb
  [choices analyze]
  (let [cprocs (map analyze choices)]
    (fn [env succeed fail]
      (letfn [(try-next [choices]
                (if (empty? choices)
                  (fail)
                  ((first choices)
                   env
                   succeed
                   #(try-next (rest choices)))))]
        (try-next cprocs)))))

(defn amb-analyze-begin
  [coll analyze]
  ((amb-analyze-seq analyze) coll))

(defn amb-analyze-let
  [exp analyze]
  (analyze (apply let->combination exp)))

(def pristine-amb-analyzors
  {:quote  amb-analyze-quote
   :set!   amb-analyze-assign
   :define amb-analyze-define
   :if     amb-analyze-if
   :lambda amb-analyze-lambda
   :begin  amb-analyze-begin
   :let    amb-analyze-let
   :amb    amb-analyze-amb})

(defn amb-eval [exp env succeed fail]
  (let [analyze (make-amb-analyze pristine-amb-analyzors)]
    ((analyze exp) env succeed fail)))

;; clojure version of amb
;; https://github.com/abeppu/toychest/blob/master/src/toychest/amb.clj

(defn amb-let-helper [bindings body]
  (if (< 0 (count bindings))
    (let [[form expression] (take 2 bindings)
          more-bindings (drop 2 bindings)
          filtered-recurse (if (= :where (first more-bindings))
                             `(when ~(second more-bindings)
                                ~(amb-let-helper (drop 2 more-bindings) body))
                             (amb-let-helper more-bindings body))
          res (if  (and (seq? expression)
                        (= 'amb (first expression)))
                `(apply concat (for [~form ~(second expression)]
                                 ~filtered-recurse))
                `(let [~form ~expression]
                   ~filtered-recurse))]
      res)
    [body]))

(defmacro amb-let [bindings body]
    "vaguely like let, except
   -- if the expression bound to a variable is of the form
      (amb col), this has the semantics that the value of the variable
      is one of the members of the collection
   -- following the binding form, we accept a vector of requirements
      each of which a vector whose first is a set of variables to which
      it applies, and whose second is an expression depending on those vars
   -- we return a lazy seq of the values produced by the let for variable
      assignments which satisfy the requirements"
  (amb-let-helper bindings body))

;; Exercise 4.35

(def an-integer-between
  '(define (an-integer-between low high)
     (require (< low high))
     (amb low (an-integer-between (+ low 1) high))))
