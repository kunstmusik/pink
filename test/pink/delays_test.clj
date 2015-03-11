(ns pink.delays-test
  (:require [pink.delays :refer :all]
            [pink.oscillators :refer [pulse]]
            [pink.config :refer [*sr* *buffer-size*]]
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

(deftest test-samp-delay
  (let [afn (samp-delay (pulse 0.0) 1)
        samps (get-samples afn 200)]
    ;(pprint samps)
    (is (float= 0.0 (aget samps 0)))
    (is (float= 1.0 (aget samps 1)))
    )
  (let [afn (samp-delay (pulse 0.0) 50)
        samps (get-samples afn 200)]
    ;(pprint samps)
    (is (float= 0.0 (aget samps 0)))
    (is (float= 1.0 (aget samps 50)))
    ))

(deftest test-frac-delay
  (let [afn (frac-delay (pulse 0.0) 1.0)
        samps (get-samples afn 200)]
    ;(pprint samps)
    (is (float= 0.0 (aget samps 0)))
    (is (float= 1.0 (aget samps 1))))
  (let [afn (frac-delay (pulse 0.0) 1.5)
        samps (get-samples afn 200)]
    ;(pprint samps)
    (is (float= 0.0 (aget samps 0)))
    (is (float= 0.5 (aget samps 1)))
    (is (float= 0.5 (aget samps 2)))
    (is (float= 0.0 (aget samps 3))))
  (let [afn (frac-delay (pulse 0.0) 1.75)
        samps (get-samples afn 200)]
    ;(pprint samps)
    (is (float= 0.0 (aget samps 0)))
    (is (float= 0.25 (aget samps 1)))
    (is (float= 0.75 (aget samps 2)))
    (is (float= 0.0 (aget samps 3)))
    )
  (let [afn (frac-delay (pulse 0.0) 5.85)
        samps (get-samples afn 300)]
    ;(pprint samps)
    (is (float= 0.0 (aget samps 0)))
    (is (float= 0.15 (aget samps 5)))
    (is (float= 0.85 (aget samps 6)))
    (is (float= 0.0 (aget samps 7)))
    )
  )
