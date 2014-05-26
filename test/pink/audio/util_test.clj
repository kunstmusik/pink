(ns pink.audio.util-test
  (:require [pink.audio.util :as util]
            [pink.audio.engine :as eng]
            )
  (:use clojure.test))


(deftest set-get-d  
  (let [a (util/create-buffer 22.0)]
    (is (= (aget ^doubles a 0) 22.0))
    (is (= (util/getd a) 22.0))))

(deftest test-shared
  (let [a (atom 0)
        tfn (util/shared (fn [] (swap! a inc)))]
    (binding [eng/*current-buffer-num* 0]
      (is (= 1 (tfn))) 
      (is (= 1 (tfn))) 
      (is (= 1 (tfn)))) 
    (binding [eng/*current-buffer-num* 1]
      (is (= 2 (tfn))) 
      (is (= 2 (tfn))) 
      (is (= 2 (tfn))))
    ))


(deftest test-reader
  (let [a (atom 1)
        rdr (util/reader a)]
    (is 1 (aget (rdr) 0)) 
    (reset! a 3.25) 
    (is 3.25 (aget (rdr) 0)) 
    ))
