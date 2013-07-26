(ns audio-seq.util-test
  (:require [audio-seq.util :as util])
  (:use clojure.test))


(deftest set-get-d  
  (let [a (util/create-buffer 22.0)]
    (is (= (aget ^doubles a 0) 22.0))
    (is (= (util/getd a) 22.0))))
