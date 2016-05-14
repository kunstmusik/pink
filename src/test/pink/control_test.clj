(ns pink.control-test
  (:require [pink.control :refer :all]
            [pink.config :refer [*sr* *buffer-size*]]
            [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            ))


(deftest test-chain
  (let [counter (atom 0)
        fn1 (fn [] (let [v (swap! counter inc)]
                     (not (> v 3))))
        fn2 (fn [] (let [v (swap! counter #(* 2 %))]
                     (not (> v 16))))
        c (chain fn1 fn2)
        ]
    (is (= 0 @counter))  
    (is (c))
    (is (= 1 @counter))  
    (is (c))
    (is (= 2 @counter))  
    (is (c))
    (is (= 3 @counter))  
    (is (c))
    (is (= 8 @counter))  
    (is (c))
    (is (= 16 @counter))  
    (is (not (c)))
    ))
