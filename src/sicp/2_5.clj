;;;; 2.5 Generic Arithmetic Operations
;;;; =================================

;;; Namespace and dependencies

(ns sicp.2-5
  (:use [clojure.math.numeric-tower :only (gcd)])
  (:use [sicp.2-4 :only (put get *dispatch-table*)]))

;;; math shortcuts

(def sqrt   #(Math/sqrt  %))
(def sin    #(Math/sin   %))
(def cos    #(Math/cos   %))
(def square #(Math/pow   %   2))
(def expt   #(Math/pow   %1 %2))
(def atan   #(Math/atan2 %1 %2))

;;; tag selector and operations

(def type-tag   first)
(def contents   second)
(def attach-tag (fn [t o] [t o]))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc      (get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (println
       "No method for these types: APPLY-GENERIC"
       (list op type-tags)))))

;;; basic arithmetic

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))

;;; scheme number package

(defn install-scheme-number-package []
  (letfn
    [(tag [x] (attach-tag 'scheme-number x))]
    (put 'add  '(scheme-number scheme-number) #(tag (+ %1 %2)))
    (put 'sub  '(scheme-number scheme-number) #(tag (- %1 %2)))
    (put 'mul  '(scheme-number scheme-number) #(tag (* %1 %2)))
    (put 'div  '(scheme-number scheme-number) #(tag (/ %1 %2)))
    (put 'make 'scheme-number                 #(tag %))
    :done))

(defn make-scheme-number [n]
  ((get 'make 'scheme-number) n))

;;; rational number package

(defn install-rational-package []
  (letfn
    [(numer [x] (first  x))
     (denom [x] (second x))
     (make-rat [n d]
               (let [g (gcd n d)]
                 [(/ n g) (/ d g)]))
     (add-rat [x y]
              (make-rat (+ (* (numer x) (denom y))
                           (* (numer y) (denom x)))
                        (* (denom x) (denom y))))
     (sub-rat [x y]
              (make-rat (- (* (numer x) (denom y))
                           (* (numer y) (denom x)))
                        (* (denom x) (denom y))))
     (mul-rat [x y]
              (make-rat (* (numer x) (numer y))
                        (* (denom x) (denom y))))
     (div-rat [x y]
              (make-rat (* (numer x) (denom y))
                        (* (denom x) (numer y))))
     (tag [x] (attach-tag 'rational x))]
    (put 'add  '(rational rational) #(tag (add-rat  %1 %2)))
    (put 'sub  '(rational rational) #(tag (sub-rat  %1 %2)))
    (put 'mul  '(rational rational) #(tag (mul-rat  %1 %2)))
    (put 'div  '(rational rational) #(tag (div-rat  %1 %2)))
    (put 'make 'rational            #(tag (make-rat %1 %2)))
    :done))

(defn make-rational [n d]
  ((get 'make 'rational) n d))

;;; rectangular package

(defn install-rectangular-package []
  (letfn
    [(real-part [z] (first  z))
     (imag-part [z] (second z))
     (make-from-real-imag [x y] [x y])
     (magnitude [z]
                (sqrt (+ (square (real-part z))
                         (square (imag-part z)))))
     (angle [z]
            (atan (imag-part z) (real-part z)))
     (make-from-mag-ang [r a]
                        [(* r (cos a)) (* r (sin a))])
     (tag [x] (attach-tag 'rectangular x))]
    (put 'real-part 'rectangular real-part)
    (put 'imag-part 'rectangular imag-part)
    (put 'magnitude 'rectangular magnitude)
    (put 'angle     'rectangular angle)
    (put 'make-from-real-imag 'rectangular
         #(tag (make-from-real-imag %1 %2)))
    (put 'make-from-mag-ang 'rectangular
         #(tag (make-from-mag-ang) %1 %2))
    :done))

;;; polar package

(defn install-polar-package []
  (letfn
    [(magnitude [z] (first  z))
     (angle     [z] (second z))
     (make-from-mag-ang [r a] [r a])
     (real-part [z]
                (* (magnitude z) (cos (angle z))))
     (imag-part [z]
                (* (magnitude z) (sin (angle z))))
     (make-from-real-imag [x y]
                          [(sqrt (+ (square x) (square y)))
                           (atan y x)])
     (tag [x] (attach-tag 'polar x))]
    (put 'real-part 'polar real-part)
    (put 'imag-part 'polar imag-part)
    (put 'magnitude 'polar magnitude)
    (put 'angle 'polar angle)
    (put 'make-from-real-imag 'polar
         #(tag (make-from-real-imag %1 %2)))
    (put 'make-from-mag-ang 'polar
         #(tag (make-from-mag-ang %1 %2)))
    :done))

(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle     [z] (apply-generic 'angle     z))

(defn make-from-real-imag [x y]
  ((get 'make-from-real-imag 'rectangular) x y))

(defn make-from-mag-ang [r a]
  ((get 'make-from-mag-ang 'polar) r a))

;;; complex number package

(defn install-complex-package []
  (letfn
    [(make-from-real-imag
      [x y]
      ((get 'make-from-real-imag 'rectangular) x y))
     (make-from-mag-ang
      [r a]
      ((get 'make-from-mag-ang 'polar) r a))
     (add-complex
      [z1 z2]
      (make-from-real-imag (+ (real-part z1) (real-part z2))
                           (+ (imag-part z1) (imag-part z2))))
     (sub-complex
      [z1 z2]
      (make-from-real-imag (- (real-part z1) (real-part z2))
                           (- (imag-part z1) (imag-part z2))))
     (mul-complex
      [z1 z2]
      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
     (div-complex
      [z1 z2]
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2))))
     (tag [z] (attach-tag 'complex z))]
    (put 'add '(complex complex) #(tag (add-complex %1 %2)))
    (put 'sub '(complex complex) #(tag (sub-complex %1 %2)))
    (put 'mul '(complex complex) #(tag (mul-complex %1 %2)))
    (put 'div '(complex complex) #(tag (div-complex %1 %2)))
    (put 'make-from-real-imag 'complex
         #(tag (make-from-real-imag %1 %2)))
    (put 'make-from-mag-ang 'complex
         #(tag (make-from-mag-ang %1 %2)))
    (put 'real-part 'complex real-part)
    (put 'imag-part 'complex imag-part)
    (put 'magnitude 'complex magnitude)
    (put 'angle     'complex angle)
    :done))

(defn make-complex-from-real-imag [x y]
  ((get 'make-from-real-imag 'complex) x y))

(defn make-complex-from-mag-ang [r a]
  ((get 'make-from-mag-ang 'complex) r a))

(do
  (install-scheme-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  :done)

;;; coercion

(def *coercion-table* (atom {}))

(defn put-coercion [t1 t2 item]
  (swap! *coercion-table* assoc-in [t1 t2] item))

(defn get-coercion [t1 t2]
  (get-in @*coercion-table* [t1 t2]))


;;; Exercise 2.79

(do
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational)
       #(and (= (first  %1) (first  %2))
             (= (second %1) (second %2))))
  (put 'equ? '(complex complex)
       #(and (= (real-part %1) (real-part %2))
             (= (imag-part %1) (imag-part %2))))
  :done)

(defn equ? [x y] (apply-generic 'equ? x y))

;; try it out

(equ? (make-scheme-number 1) (make-scheme-number 1))
(equ? (make-scheme-number 1) (make-scheme-number 2))
(equ? (make-rational 1 2)    (make-rational 1 2))
(equ? (make-rational 1 2)    (make-rational 1 3))
(equ? (make-complex-from-real-imag 1 2)
      (make-complex-from-real-imag 1 2))
(equ? (make-complex-from-real-imag 1 2)
      (make-complex-from-real-imag 1 3))


;;; Exercise 2.81

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc      (get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (if (and (= (count args) 2) (apply not= type-tags))
        (let [type1  (first  type-tags)
              type2  (second type-tags)
              a1     (first  args)
              a2     (second args)
              t1->t2 (get-coercion type1 type2)
              t2->t1 (get-coercion type1 type2)]
          (cond t1->t2 (apply-generic op (t1->t2 a1) a2)
                t2->t1 (apply-generic op a1 (t2->t1 a2))
                :else  (print-str "No method for these types"
                                  (list op type-tags))))
        (print-str "No method for these types"
                   (list op type-tags))))))

;; try it out

(defn exp [x y] (apply-generic 'exp x y))
(exp (make-scheme-number 2) (make-scheme-number 10))

;;; Exercise 2.82

(defn apply-generic+ [op & args]
  (let
    [type-tags (map type-tag args)
     proc      (get op type-tags)]
    ;;
    (letfn
      [(coerce-to-type
        [coll type]
        (cond (nil? (seq coll))
              '()
              ;;
              (= (type-tag (first coll)) type)
              (cons (first coll)
                    (coerce-to-type (rest coll) type))
              ;;
              :else
              (let
                [t1->t2 (get-coercion (type-tag (first coll)) type)]
                (if t1->t2
                  (do
                    (cons (t1->t2 (first coll))
                          (coerce-to-type (rest coll) type)))
                  (cons (first coll)
                        (coerce-to-type (rest coll) type))))))
       ;;
       (apply-coercion
        [coll]
        (loop [head coll]
          (if-not (seq head)
            (print-str "No method for these types"
                       (list op type-tags))
            (let [coerced-list
                  (coerce-to-type coll (type-tag (first head)))
                  proc
                  (get op (map type-tag coerced-list))]
              (if proc
                (apply proc (map contents coerced-list))
                (recur (rest head)))))))]
      ;; body
      (if proc
        (apply proc (map contents args))
        (apply-coercion args)))))

;; try it out

(defn scheme-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))

(defn add-complex [z1 z2]
  (make-complex-from-real-imag (+ (real-part z1) (real-part z2))
                               (+ (imag-part z1) (imag-part z2))))

(do
  (put-coercion 'scheme-number
                'complex
                scheme-number->complex)
  (put 'add+
       '(complex complex complex)
       #(add-complex (add-complex %1 %2) %3))
  :done)

(apply-generic+ 'add+
                (make-complex-from-real-imag 1 2)
                (make-scheme-number 2)
                (make-scheme-number 3))
