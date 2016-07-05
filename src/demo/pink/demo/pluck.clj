(ns pink.demo.pluck
  (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.filter]
             [pink.instruments.pluck :refer :all] 
             ))

(defn panks
  [& args]
  (->
    (apply karplus args)
    (pan 0.1)))

(comment
  
  (start-engine)
  
  (add-audio-events
    (i panks 0 0.5 0.25 220 6.0))

  (add-audio-events
    (i panks 0 0.5 0.25 220 3.0)
    (i panks 3.0 0.5 0.25 330 3.0)
    (i panks 5.0 0.5 0.25 440 3.0)
    (i panks 5.2 0.5 0.25 400 3.0))

  (clear-engine)

  )
