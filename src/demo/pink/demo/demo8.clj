;; Test of Events 

(ns pink.demo.demo8
  (:require [pink.engine :refer :all]
            [pink.envelopes :refer [env exp-env adsr xar]]
            [pink.oscillators :refer [sine oscil oscili oscil3]]
            [pink.gen :refer [gen-sine gen10]]
            [pink.util :refer :all]
            [pink.config :refer :all]
            [pink.event :refer :all] ))

(def sine256 (gen-sine 128))

(def table0 (gen10 65536 1 0.5 0.25 0.125 0.06125 ))
;(def table0 (gen10 65536 1 1))

(defn table-synth [freq]
  (println "Truncating...")
  (mul
     (oscil 0.05 freq sine256)
     (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])))

(defn table-synth-interp [freq]
  (println "Interpolating...")
  (mul
     ;(oscili 0.05 freq sine256)
     (oscili 0.05 freq table0)
     ;(sine 0.05 freq)
     (env [0.0 0.0 0.05 1 0.02 0.8 5.2 0.8 0.2 0])))

(defn table-synth-cubic [freq]
  (println "Cubic...")
  (mul
     (oscil3 0.05 freq sine256)
     (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])))

;(time-gen (table-synth-interp 440.0))

(comment

  (def e (engine-create))
  (engine-start e)

  (let [eng-events (audio-events e
                       (map #(event table-synth-interp 0.25 (* 110 %)) (range 1 10)))]

      (engine-add-post-cfunc e (event-list-processor 
                                 (event-list eng-events *buffer-size* *sr*)))
    
    ) 

  (engine-stop e)
  (engine-clear e)
  (engine-kill-all)

  (let [e (engine-create)
        eng-events 
        (audio-events e
                       (event table-synth 0.0 440.0) 
                       (event table-synth 0.0 550.0) 
                       ;(map #(event table-synth-interp 0.25 (* 110 %)) (range 1 36)) 

                       (event table-synth-interp 1.0 440.0) 
                       (event table-synth-interp 1.0 550.0)

                       (event table-synth-cubic 2.0 440.0) 
                       (event table-synth-cubic 2.0 550.0)

                       ;(event table-synth-interp 1.0 440.0) 
                       ;(event table-synth-interp 1.0 550.0)
                       )
        ]
    
      (engine-start e)
      (engine-add-events e eng-events)

      (Thread/sleep 3000)
      (engine-stop e)
      (engine-clear e))


  )

