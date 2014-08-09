;; Test of Events 

(ns pink.demo.demo9
  (:require [pink.engine :as eng]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             ))

(comment

  (def e (eng/engine-create :nchnls 2))
  (eng/engine-start e)

  (def num-notes 5)
  (let [eng-events (engine-events e
                       (map #(event horn (* % 0.5)  (/ 0.75 (+ 1 %)) (* 220 (+ 1 %)) (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]

      (eng/engine-add-afunc e (eng-events-runner eng-events))
    
    ) 

  (eng/engine-stop e)
  (eng/engine-clear e)
  (eng/engine-kill-all)


  )

