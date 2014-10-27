(ns pink.benchmark.benchmark
  (:import [pink.benchmark Benchmark Benchmark$Phasor])
  (:require [pink.config :refer :all]
            [pink.oscillators :refer :all]
            [clojure.pprint :refer [pprint]]
            ))

(defn run-java-phasor-test
  []
  (let [p (Benchmark$Phasor. 440.0 0.0 *sr* *buffer-size*)]
    (println "Java Phasor Test")
    (doseq [_ (range 5)] 
      (time 
        (doseq [_ (range 1000000)]
          (.tick p))))))



(run-java-phasor-test)

(defn run-pink-phasor-test
  []
  (let [p (phasor 440.0 0.0)]
    (println "Pink Phasor Test")
    (doseq [_ (range 5)] 
      (time 
        (doseq [_ (range 1000000)]
          (p))))))


(run-pink-phasor-test)
