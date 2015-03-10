(ns pink.delays-test
  (:require [pink.delays :refer :all]
            [pink.oscillators :refer [pulse]]
            [pink.config :refer [*sr* *buffer-size*]]
            [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            ))

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
    (is (= 0.0 (aget samps 0)))
    (is (= 1.0 (aget samps 1)))
    )
  (let [afn (samp-delay (pulse 0.0) 50)
        samps (get-samples afn 200)]
    ;(pprint samps)
    (is (= 0.0 (aget samps 0)))
    (is (= 1.0 (aget samps 50)))
    )
  )

