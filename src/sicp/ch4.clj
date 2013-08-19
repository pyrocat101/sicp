;;;; Chapter 4: Metalinguistic Abstraction
;;;; =====================================

;;; Namespace and dependencies

(ns sicp.ch4)

;;; define the most powerful language in a page of code

(defn my-eval [exp env]
  (cond (self-eval?   exp) exp
        (variable?    exp) (lookup-variable-value exp env)
        (quoted?      exp) (text-of-quotation exp)
        (assignment?  exp) (eval-assignment exp env)
        (definition?  exp) (eval-definition exp env)
        (if?          exp) (eval-if exp env)
        (lambda?      exp) (make-procedure (lambda-params exp)
                                           (lambda-body exp)
                                           env)
        (begin?       exp) (eval-sequence (begin-actions exp) env)
        (cond?        exp) (my-eval (cond->if exp) env)
        (application? exp) (my-apply (my-eval (operator exp) env)
                                     (list-of-values (operands exp) env))
        :else "Unknown expression-type -- EVAL" exp))

(defn my-apply [proc args]
  (cond (primitive-proc? proc) (apply-primitive-proc proc args)
        (compound-proc?  proc) (eval-sequence
                                (proc-body proc)
                                (extend-env
                                 (proc-params proc)
                                 args
                                 (proc-env proc)))
        :else "Unknown procedure type -- APPLY" proc))

(defn list-of-values [exps env]
  (map #(my-eval % env) exps))

(defn eval-if [exp env]
  (if (my-eval (if-predicate exp) env)
    (my-eval (if-consequent  exp) env)
    (mu-eval (if-alternative exp) env)))

(defn eval-sequence [exps env]
  (doseq [exp exps] (my-eval exp env)))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assigment-value exp) env)
                       env)
  :ok)

(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  :ok)
