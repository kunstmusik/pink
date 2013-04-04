(ns audio-seq.core
  (:require [audio-seq.engine :as engine]
            [audio-seq.envelopes :refer [env]]
            [audio-seq.oscillators :refer [sine]]
            [audio-seq.util :refer [mix mul]]))


    
(defn audio-block3 [x]
  (mul
    (apply mix 
     (map #(sine (* % 60) 0)
        (take x (iterate inc 1))))
    (env [0.0 0.0 0.05 1 0.05 0.9 0.5 0.9 0.5 0])))

(defn demo3 [x] (engine/run-audio-block (audio-block3 x)))

