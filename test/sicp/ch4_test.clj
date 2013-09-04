(ns sicp.ch4_test
  (:require [clojure.test :refer :all]
            [sicp.ch4 :refer :all]))

(declare ^:dynamic *tmp*)
(deftest test-list-of-values-ltr
  (binding [*tmp* (atom 1)]
    (list-of-values-ltr '((reset! *tmp* 6)
                          (swap! *tmp* inc)))
    (is (= @*tmp* 7))))
