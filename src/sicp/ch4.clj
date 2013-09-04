;;;; Chapter 4: Metalinguistic Abstraction
;;;; =====================================

;;; Namespace and dependencies

(ns sicp.ch4
  (:require [clojure.core.match :refer (match)]))

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
  (extend [this bindings])
  (lookup [this var])
  (set! [this var value])
  (define! [this var value]))

(deftype Env [bindings enclose]
  Environmental
  (extend [this bindings]
    (Env. (atom bindings) this))
  (lookup [this var]
    (loop [env this]
      (let [bindings @(.bindings env)
            enclose  (.enclose env)]
        (if (contains? bindings var)
          (get bindings var)
          (if-not enclose
            (throw (Exception.
                    (print-str "Unbound variable:" var)))
            (recur enclose))))))
  (set! [this var value]
    (loop [env this]
      (let [bindings (.bindings env)]
        (if (contains? @bindings var)
          (swap! bindings assoc var value)
          (if-not enclose
            (throw (Exception.
                    (print-str "Unbound variable:" var)))
            (recur enclose))))))
  (define! [this var value]
    (swap! (.bindings this) assoc var value)))

(defn make-env
  ([bindings]
     (make-env bindings nil))
  ([bindings enclose]
     (Env. (atom bindings) enclose)))

;; procedure and primitive

(defprotocol Applicable
  "Something that can be applied"
  (apply [this args env eval]))

(deftype Procedure [params body env]
  Applicable
  (apply [this args env eval]
    (let [env (sicp.ch4/extend (.env this)
                               (zipmap (.params this) args))]
      ((eval-seq eval) (.body this) env))))

(deftype Primitive [fn]
  Applicable
  (apply [this args env eval]
    (clojure.core/apply (.fn this) args)))

(defn applicative-apply
  [exp env eval]
  (let [[proc & args] (map #(eval % env) exp)]
    (sicp.ch4/apply proc args env eval)))

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
          (variable? exp)  (.lookup env exp)
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
  (sicp.ch4/set! env v (eval o)))

(defn eval-define
  [[v o] env eval]
  (sicp.ch4/define! env v (eval o)))

(defn eval-if
  [[pred conseq alt] env eval]
  (if (eval pred env)
    (eval conseq env)
    (eval alt env)))

(defn eval-lambda
  [[params & body] env _]
  (Procedure. params body env))

(defn eval-begin
  [exp env eval]
  ((eval-seq eval) exp env))

(defn cond->if
  ([] false)
  ([[[pred act] & next :as clauses]]
     (if (= pred 'else)
       (if (empty? next)
         act
         (throw (Exception.
                 (print-str "ELSE clause isn't last:" clauses)))))))

(defn eval-cond
  [exp env eval]
  (eval (cond->if exp) env))

;; basic primitives

(def pristine-primitives
  (let [m {'car   first
           'cdr   rest
           'cons  cons
           'null? nil?
           '+     +
           '-     -
           '*     *
           '/     /}]
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

;; (def eval-1 (make-eval pristine-special-forms applicative-apply))
;; (def env (make-env pristine-primitives))

;; Exercise 4.1

(defn list-of-values-ltr [exps]
  (if (empty? exps)
    '()
    (#(cons % (list-of-values-ltr (rest exps)))
     (eval (first exps)))))

;; Exercise 4.2

;; Louis is an idiot.

;; Exercise 4.5
