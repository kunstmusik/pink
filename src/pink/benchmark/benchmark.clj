(ns pink.benchmark.benchmark
  (:import [pink.benchmark Benchmark$Phasor])
  (:require [pink.config :refer :all]
            [pink.oscillators :refer :all]
            [pink.util :refer :all]
            [clojure.pprint :refer [pprint]]
            ))

(defn run-phasor-test
  []
  (let [p (Benchmark$Phasor. 440.0 0.0 *sr* *buffer-size*)]
    (println "Java Phasor Test")
    (doseq [_ (range 5)] 
      (time 
        (doseq [_ (range 1000000)]
          (.tick p)))))
  (let [p (phasor 440.0 0.0)]
    (println "Pink Phasor Test")
    (doseq [_ (range 5)] 
      (time 
        (doseq [_ (range 1000000)]
          (p))))))

(defn run-mul-test
  []
  (let [p (mul 440.0 2.0)]
    (println "Pink Mul Test")
    (doseq [_ (range 5)] 
      (time 
        (doseq [_ (range 1000000)]
          (p)))))
  (let [p (mul2 440.0 2.0)]
    (println "Pink Mul2 Test")
    (doseq [_ (range 5)] 
      (time 
        (doseq [_ (range 1000000)]
          (p))))))


(comment 

(run-mul-test)
(run-phasor-test)

)

