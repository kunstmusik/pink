(ns compose.audio.envelopes-test
  (:use [compose.audio.envelopes])
  (:use clojure.test))


(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))


(def pts [0.0 0.001 0.05 1.0 0.3 0.001])
;(def t (exp-env pts))
;(def pts-data (make-exp-env-data pts))
;(def pts-data2 (make-env-data pts))

(deftest test-make-env-data
  (with-private-fns [compose.audio.envelopes [make-env-data]]
    (let [a (make-env-data pts)]
      (is (= 2205.0 (first (first a))))
      (is (= 2 (count a))) 
      )))

