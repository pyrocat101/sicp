;;;; 3.3 Modeling with Mutable Data
;;;; ==============================

;;; Namespace and dependencies

(ns sicp.3-3)


;;; Exercise 3.19

(defn cycle [coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (concat s (cycle s)))))

(defn cycle? [coll]
  (loop [x (drop 1 coll)
         y (drop 2 coll)]
    (cond (or (nil? (seq x))
              (nil? (seq y)))
          false
          (identical? (first x)
                      (first y))
          true
          :else
          (recur (drop 1 x)
                 (drop 2 y)))))

(cycle? (cycle '(666 777)))
(cycle? '(666 777 666 777))
