;; Test of Events 

(ns pink.demo.demo7
  (:require [pink.engine :as eng]
            [pink.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.oscillators :refer [oscil sine2]]
            [pink.util :refer [mix mul swapd! sum const create-buffer getd setd! arg shared let-s reader]]
            [pink.event :refer :all] ))


(defn table-synth [freq]
  (mul
     (oscil 0.5 freq)
     (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])))

(comment

  (let [e (eng/engine-create)
        eng-events 
        (engine-events e
                       (event table-synth 0.0 440.0) 
                       (event table-synth 0.5 550.0) 
                       (event table-synth 1.0 660.0) 
                       (event table-synth 1.5 880.0))
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (event-list-processor eng-events))

      (Thread/sleep 2200)
      (eng/engine-stop e)
      (eng/engine-clear e))


  )

