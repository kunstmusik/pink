(ns audio-seq.demo.demo1
  (:require [audio-seq.engine :as eng]
            [audio-seq.envelopes :refer [env]]
            [audio-seq.oscillators :refer [sine]]
            [audio-seq.util :refer [mix mul const]]))


(defn simple-synth [freq]
  (mul
    (mix 
      (sine freq 0)
      (mul (const 0.5) (sine (* 2 freq) 0))
      (mul (const 0.25) (sine (* 3 freq) 0))
      (mul (const 0.125) (sine (* 4 freq) 0)))
    (mul
      (const 0.25)
      (env [0.0 0.0 0.02 1 0.02 0.9 0.2 0.9 0.2 0]))))


(defn demo [e]
  (let [melody (take (* 4 4) (cycle [220 330 440 330]))
        dur 0.25]
    (loop [[x & xs] melody]
      (println x)
      (when x
        (send e #(assoc % :audio-funcs (conj (% :audio-funcs) (simple-synth x) (simple-synth (* 2 x)))))
        (Thread/sleep (* 1000 dur))
        (recur xs)))))

;;

(comment

(let [e (eng/engine-create)]
  (eng/engine-start e)
  (demo e)
  (Thread/sleep 500)
  (eng/engine-stop e))
     
)


