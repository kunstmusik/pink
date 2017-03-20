(ns pink.demo.demo-node
 (:require [pink.engine :refer :all]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             [pink.util :refer [mul try-func]]
             [pink.oscillators :refer :all]
             [pink.envelopes :refer [env]]
             [pink.node :refer :all]))

(comment

  (def e (engine-create :nchnls 2))
  (engine-start e)

  ;(require '[pink.noise :refer :all])
  ;(engine-add-afunc e (white-noise))

  (def root-node (audio-node :channels 2))
  (engine-add-afunc e root-node)
 
  (def my-score 
    (let [num-notes 5] 
      (node-events root-node 
                   (map #(event horn (* % 0.5)  
                                (/ 0.75 (+ 1 %)) 
                                (* 220 (+ 1 %)) 
                                (- (* 2 (/ % (- num-notes 1)))  1)) 
                        (range num-notes)))))

  (engine-add-events e my-score) 

  ;(def s (sine 440.0))
  ;(node-add-func root-node s)
  ;(node-remove-func root-node s)

  (engine-stop e)
  (engine-clear e)
  (engine-kill-all)


  )

