(ns pink.processes-test
  (:require [pink.processes :refer [process wait]]
            [pink.config :refer :all]
            [clojure.test :refer :all]))

(deftest test-process
  (let [counter (atom 0) 
        p (process
            (reset! counter 1) 
            (wait 1.0)
            (reset! counter 2))
        num-wait (long (Math/round (+ 0.4999999 (/ *sr* *buffer-size*)))) 
        ]
    (is (= @counter 0)) 
    (p)
    (is (= @counter 1))
    (loop [c 2]
      (if (p)
        (recur (inc c))
        (is (= c num-wait))))

    (is (= @counter 2)))) 

(deftest test-process-loop
  (let [counter (atom 0) 
        p (process
            (loop [a 0]
              (wait 1.0)
              (reset! counter (inc a)) 
              (recur (inc a))))
        num-wait (Math/round (+ 0.4999999 (/ *sr* *buffer-size*)))
        num-wait2 (dec num-wait)
        ]
    (is (= @counter 0)) 
    (loop [c 0]
      (if (= @counter 0) 
        (do 
          (p)
          (recur (inc c)))
        (do 
          (is (= c num-wait))
          (is (= @counter 1))
          )))
    ;; the last wait from previous loop starts the next wait,
    ;; so counting from 1 here
    (loop [c 1]
      (if (= @counter 1)
        (do 
          (p)
          (recur (inc c)))
        (do 
          ;; checking num-wait 2, which is one buffer less
          ;; than num-wait, due to leftover samples from
          ;; previous wait time
          (is (= c num-wait2))
          (is (= @counter 2))
          )))))   
