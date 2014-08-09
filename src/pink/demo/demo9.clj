;; Test of Events 

(ns pink.demo.demo9
  (:require [pink.engine :as eng]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             ))

(comment

  (def e (eng/engine-create))
  (eng/engine-start e)

  (let [eng-events (engine-events e
                       (map #(event horn (* % 0.5)  (/ 0.75 %) (* 220 %)) 
                            (range 1 6)))]

      (eng/engine-add-afunc e (eng-events-runner eng-events))
    
    ) 

  (eng/engine-stop e)
  (eng/engine-clear e)
  (eng/engine-kill-all)


  )

