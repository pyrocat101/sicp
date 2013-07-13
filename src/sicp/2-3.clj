; 2.3 Symbolic Data
; =================

; Namespace and dependencies

(ns sicp.2-3
  (:require [clojure.math.numeric-tower :as math]))


; Exercise 2.54

(defn equal? [list1 list2]
  (cond (or (= list1 '()) (= list2 '()))   true
        (not= (first list1) (first list2)) false
        :else (recur (rest list1) (rest list2))))

(equal? '(a b c d) '(a b c d))
(equal? '(a b c d) '(a c b d))


; Exercise 2.55

; (car ''abracadabra)
; -> (car (quote (quote abracadabra)))
; -> quote
