(ns pink.demo.pluck
  (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.instruments.pluck :refer :all] 
             ))

(defn panks
  [& args]
  (->
    (apply pluck args)
    (pan 0.1)))

(comment
  
  (start-engine)
  
  (add-audio-events
    (i panks 0 3.0 0.25 100))

  (add-audio-events
    (i panks 0 0.5 0.25 200)
    (i panks 0 0.5 0.25 300)
    (i panks 0 0.5 0.25 350))

  (add-audio-events
    (i panks 0 0.5 0.25 220)
    (i panks 3.0 0.5 0.25 330)
    (i panks 5.0 0.5 0.25 440)
    (i panks 5.2 0.5 0.25 400))

  (add-audio-events
    (i panks 0 0.5 0.25 440))


  (clear-engine)

  (stop-engine)

  )
