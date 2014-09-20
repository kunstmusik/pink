(ns pink.demo.demo-band-limited
 (:require [pink.engine :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.oscillators :refer [blit-saw blit-square]]
             [pink.envelopes :refer [env]]
             [pink.util :refer [mul]]
             [pink.node :refer :all]))

(defn instr-saw
  [amp freq loc]
  (pan 
    (mul (env [0 0 0.02 0.25 0.3 0.25 0.05 0.0])
         (blit-saw freq))
    loc))

(defn instr-square
  [amp freq loc]
  (pan 
    (mul (env [0 0 0.02 0.25 0.3 0.25 0.05 0.0])
         (blit-square freq))
    loc))

;(def a (blit-square 440))
;(require '[clojure.pprint :refer [pprint]])
;(pprint (a))

(comment

  (def e (engine-create :nchnls 2))
  (engine-start e)

  (def root-node (create-node :channels 2))
  (engine-add-afunc e (node-processor root-node))

  (def my-score 
    (let [num-notes 10] 
      (node-events root-node 
                   (map #(event instr-saw (* % 0.5)  
                                (/ 0.75 (+ 1 %)) 
                                (* 220 (+ 1 %)) 
                                (- (* 2 (/ % (- num-notes 1)))  1)) 
                        (range num-notes)))))

  (engine-add-events e my-score) 

  (node-add-afunc
    root-node 
    (instr-saw 0.25 (env [0.0 220 0.1 4000 0.0001 220 0.1 4000]) 0.0))


  (def my-score2
    (let [num-notes 10] 
      (node-events root-node 
                   (map #(event instr-square (* % 0.5)  
                                (/ 0.75 (+ 1 %)) 
                                (* 220 (+ 1 %)) 
                                (- (* 2 (/ % (- num-notes 1)))  1)) 
                        (range num-notes)))))

  (engine-add-events e my-score2) 

  (node-add-afunc
    root-node 
    (instr-square 0.25 (env [0.0 220 0.1 4000 0.0001 220 0.1 4000]) 0.0))

  (engine-stop e)
  (engine-clear e)
  (engine-kill-all)


  )
