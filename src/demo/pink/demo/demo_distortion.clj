(ns pink.demo.demo-distortion
  (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.instruments.pluck :refer :all] 
             [pink.effects.distortion :refer :all] 
             [pink.util :refer [!*!]]
             ))

(defn guit1
  [pch sat]
  (->
    (pluck 1.0 pch) 
    (distort sat)
    (pan 0.0)))


(defn guit2
  [pch sat]
  (->
    (pluck 1.0 pch) 
    (distort1 sat 1 0.8 0)
    (pan 0.0)))

(comment
  
  (start-engine)
  
  (add-audio-events
    (i guit1 0 1.0 200 4)
    (i guit1 1 1.0 400 2)
    (i guit1 2 1.0 800 1))

  (add-audio-events
    (i guit2 0 1.0 200 1)
    (i guit2 3 1.0 200 4)
    (i guit2 6 1.0 200 8))

  (clear-engine)

  (stop-engine)

  )
