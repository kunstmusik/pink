(ns pink.demo.demo-node
 (:require [pink.engine :as eng]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             [pink.util :refer [mul]]
             [pink.oscillators :refer [oscil3 sine-table]]
             [pink.envelopes :refer [env]]
             [pink.node :refer :all]
             ) 
  )

(comment

  (def e (eng/engine-create :nchnls 2))
  (eng/engine-start e)

  (def root-node (create-node :channels 2))
  (eng/engine-add-afunc e (node-processor root-node))
 
  (def num-notes 5)
  (let [n-events 
        (node-events root-node 
                       (map #(event horn (* % 0.5)  
                                    (/ 0.75 (+ 1 %)) 
                                    (* 220 (+ 1 %)) 
                                    (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]
      (eng/engine-add-post-cfunc e (event-list-processor n-events))) 


  (eng/engine-stop e)
  (eng/engine-clear e)
  (eng/engine-kill-all)


  )

