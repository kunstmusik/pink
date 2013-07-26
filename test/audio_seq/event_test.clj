(ns audio-seq.event-test
  (:require [audio-seq.event :refer :all]
            [clojure.test :refer :all]
            )
  )

(defn test-audio-func [])

(deftest event-test
  (let [evt (event test-audio-func 0.5 1.0 4.0 :test)]
   (is (= 0.5 (:start evt))) 
   (is (= 1.0 (:duration evt))) 
   (is (= [4.0 :test] (:init-args evt))) 
   (is (= test-audio-func (:init-func evt))) 
    ))
