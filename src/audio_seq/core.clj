(ns audio-seq.core
  (:require [audio-seq.engine :as eng]
            [audio-seq.envelopes :refer [env]]
            [audio-seq.oscillators :refer [sine]]
            [audio-seq.util :refer [mix mul const]]))


    
(defn audio-block3 [x]
  (mul
    (apply mix 
     (map #(sine (* % 60) 0)
        (take x (iterate inc 1))))
    (env [0.0 0.0 0.05 1 0.05 0.9 0.5 0.9 0.5 0])))


(defn simple-synth [freq]
  (mul
    (mix 
      (sine freq 0)
      (mul (const 0.5) (sine (* 2 freq) 0))
      (mul (const 0.25) (sine (* 3 freq) 0))
      (mul (const 0.125) (sine (* 4 freq) 0)))
    (mul
      (const 0.25)
      (env [0.0 0.0 0.02 1 0.05 0.9 0.2 0.9 0.2 0]))))

(defn demo3 [x] (engine/run-audio-block (audio-block3 x)))

(defn demo4 []
  (let [melody (take (* 4 4) (cycle [220 330 440 330]))
        dur 0.25
        e (eng/engine-create)]
    (eng/engine-start e)
    (loop [[x & xs] melody]
      (println x)
      (when x
        (send e assoc :audio-funcs (conj (@e :audio-funcs) (simple-synth x)))
        (Thread/sleep (* 1000 dur))
        (recur xs)))
    (eng/engine-stop e)))

     
     

