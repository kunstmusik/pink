(ns pink.filters-test
  (:require [pink.filters :refer :all]
            [pink.oscillators :refer [pulse]]
            [pink.config :refer [*sr* *buffer-size*]]
            [pink.util :refer [arg]]
            [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            ))

(defn float= [^double x ^double y]
  (<= (Math/abs (- x y)) 0.00001))

(defn get-samples 
  ^doubles [afn ^long num-samples]
  (let [out ^doubles (double-array num-samples)]
    (loop [^doubles vs (afn) index 0 buffer 0]
      (let [q (quot index (long *buffer-size*))
            r (rem index (long *buffer-size*))] 
        (if (< index num-samples)
          (if (> q buffer)
            (recur (afn) index q)
            (do 
              ;(println q ":" r ": " (aget vs r))
              (aset out index (aget vs r))
              (recur vs (inc index) buffer)))
          out)))))

(deftest test-one-zero
  (let [afn (one-zero (pulse 0.0) 0.5)
        samps (get-samples afn 20)]
    ;(pprint samps)
    (is (float= 1.0 (aget samps 0)))
    (is (float= -0.5 (aget samps 1)))
    (is (float= 0.0 (aget samps 2)))
    )
  (let [afn (one-zero (pulse 0.0) (arg 0.5))
        samps (get-samples afn 20)]
    ;(pprint samps)
    (is (float= 1.0 (aget samps 0)))
    (is (float= -0.5 (aget samps 1)))
    (is (float= 0.0 (aget samps 2)))
    ))

(deftest test-one-pole
  (let [afn (one-pole (pulse 0.0) 0.5)
        samps (get-samples afn 20)]
    ;(pprint samps)
    (is (float= 1.0 (aget samps 0)))
    (is (float= 0.5 (aget samps 1)))
    (is (float= 0.25 (aget samps 2)))
    )
  (let [afn (one-pole (pulse 0.0) (arg 0.5))
        samps (get-samples afn 20)]
    ;(pprint samps)
    (is (float= 1.0 (aget samps 0)))
    (is (float= 0.5 (aget samps 1)))
    (is (float= 0.25 (aget samps 2)))
    ))
