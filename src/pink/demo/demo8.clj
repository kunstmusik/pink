;; Test of Events 

(ns pink.demo.demo8
  (:require [pink.audio.engine :as eng]
            [pink.audio.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.audio.oscillators :refer [oscil oscili]]
            [pink.audio.gen :refer [gen-sine]]
            [pink.audio.util :refer [mix mul swapd! sum const create-buffer getd setd! arg shared let-s reader]]
             [pink.event :refer :all] ))

(def sine256 (gen-sine 256))

(defn table-synth [freq]
  (println "Truncating...")
  (mul
     (oscil 0.05 freq sine256)
     (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])))

(defn table-synth-interp [freq]
  (println "Interpolating...")
  (mul
     (oscili 0.05 freq sine256)
     (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])))

(comment

  (let [e (eng/engine-create)
        eng-events 
        (engine-events e
                       (event table-synth 0.0 440.0) 
                       (event table-synth 0.0 550.0) 
                       (event table-synth-interp 1.0 440.0) 
                       (event table-synth-interp 1.0 550.0)
                       
                       )
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (eng-events-runner eng-events))

      (Thread/sleep 2200)
      (eng/engine-stop e)
      (eng/engine-clear e))


  )

