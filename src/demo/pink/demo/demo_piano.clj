(ns pink.demo.demo-piano
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.instruments.piano :refer :all]
             [pink.util :refer [mul try-func]]
             [pink.filters :refer :all]
             [pink.node :refer :all]
             [pink.space :refer :all]
             [pink.config :refer :all]
             ))

(defn instr 
  [amp key-num]
  (->
    (piano :duration *duration* :keynum key-num)
    (pan 0.0)
    )
  )


(comment
  
  (start-engine)

  (add-audio-events
    (i instr 0.0 0.4 0.25 60)
    (i instr 0.5 0.4 0.25 64))


  )
