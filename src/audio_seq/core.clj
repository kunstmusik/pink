(ns audio-seq.core
  (:use [audio-seq.engine :as engine])
  (:use audio-seq.util)
  (:use audio-seq.oscillators)
  (:use audio-seq.envelopes))


    
(defn audio-block3 [x]
  (mul
    (apply mix 
     (map #(sine (* % 60) 0)
        (take x (iterate inc 1))))
    (env [0.0 0.0 0.05 1 0.05 0.9 0.5 0.9 0.5 0])))

(defn demo3 [x] (engine/run-audio-block (audio-block3 x)))

