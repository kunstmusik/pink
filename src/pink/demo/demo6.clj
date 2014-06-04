;; Test of Events 

(ns pink.demo.demo6
  (:require [pink.audio.engine :as eng]
            [pink.audio.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.audio.oscillators2 :refer [sine sine2]]
            [pink.audio.util :refer [mix mul swapd! sum const create-buffer getd setd! arg shared let-s reader]]
             [pink.event :refer :all] ))


(defn fm-synth [freq]
  (let-s [e (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])] 
    (mul
        (sine2 (sum freq (mul e 440 (sine freq))))
        (mul 0.4 e))))

;; test design work
;; mutable value will be held in an atom
;; reader will be the audio-func to read from the atom

(def index (atom 1))
(def t (reader index))
(reset! index 3.25)

(defn fm-bell [freq]
  (
   ;let-s [e (exp-env [0.0 0.00001 0.05 1.0 3 0.000001])] 
   let-s [e (xar 0.0001 1.3)] 
    (mul
        (sine2 (sum freq (mul freq t (sine (* 4.77 freq)))))
        (mul 0.1 e))))

;;

(comment

  (let [e (eng/engine-create)
        eng-events 
        (engine-events e
                       (event fm-bell 0.0 440.0) 
                       (event fm-bell 0.5 550.0) 
                       (event fm-bell 1.0 660.0) 
                       (event fm-bell 1.5 880.0))
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (eng-events-runner eng-events))

      (Thread/sleep 2200)
      (eng/engine-stop e)
      (eng/engine-clear e))


  )

