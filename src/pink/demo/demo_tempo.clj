(ns pink.demo.demo-tempo
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             [pink.util :refer [mul try-func]]
             [pink.oscillators :refer :all]
             [pink.envelopes :refer :all]
             [pink.filters :refer :all]
             [pink.node :refer :all]
             [pink.space :refer :all]
             ))

(defn instr
  [amp pitch]
  (->
    (blit-saw pitch)
    (moogladder 2000 0.5)
    (mul amp (adsr 0.02 0.02 0.95 0.1))
    (pan 0.0)))

(comment
  
  (start-engine)

  (add-audio-events
    (i instr 0.0 0.4 0.25 400)
    (i instr 0.5 0.4 0.25 800))

  (set-tempo 30)
  (set-tempo 60)
  (set-tempo 120)

  )
