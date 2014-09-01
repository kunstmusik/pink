(ns pink.envelopes-test
  (:require [pink.envelopes :refer :all])
  (:require [clojure.test :refer :all]))


(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))


(def pts [0.0 0.001 0.05 1.0 0.3 0.001])
;(def t (exp-env pts))
;(def pts-data (make-exp-env-data pts))
;(def pts-data2 (make-env-data pts))

(deftest test-make-env-data
  (with-private-fns [pink.envelopes [make-env-data]]
    (let [[start-val & pts] (make-env-data pts #(/ (- %1 %2) %3))]
      (is (= 2205.0 (ffirst pts)))
      (is (= 2 (count pts))) 
      )))

