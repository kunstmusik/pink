(ns pink.demo.demo-band-limited
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.oscillators :refer [blit-saw blit-square]]
             [pink.envelopes :refer [env xar]]
             [pink.util :refer [mul sum let-s]]
             [pink.node :refer :all]
             [pink.filters :refer [tone butterlp]]
             [pink.delays :refer [adelay]]
             ))

(defn instr-saw
  [amp freq loc]
  (let-s [e (xar 0.01 1.0)] 
    (pan 
      (mul e
           (butterlp (blit-saw freq) 
                 (sum 100 (mul e 400))))
      loc)))

(defn instr-square
  [amp freq loc]
  (let-s [e (xar 0.01 1.0)] 
    (pan 
      (mul e
           (butterlp (blit-square freq) 
                 (sum 100 (mul e 400))))
      loc)))

;(def a (instr-saw 0.1 440 0.0))
;(require '[clojure.pprint :refer [pprint]])
;(pprint (a))

(comment

  (start-engine)

  (def root-node (create-node :channels 2))
  (add-afunc (node-processor root-node))

  ;(def root-node (create-node :channels 1))
  ;(def delayed-audio-node
  ;  (let-s [afn (node-processor root-node)]
  ;    (sum afn (adelay afn 0.25))))

  ;(engine-add-afunc e delayed-audio-node)

  (def my-score 
    (let [num-notes 10] 
      (node-events root-node 
                   (map #(event instr-saw (* % 0.25)  
                                (/ 0.75 (+ 1 %)) 
                                (* 220 (+ 1 %)) 
                                (- (* 2 (/ % (- num-notes 1))) 1)) 
                        (range num-notes)))))

  (add-events my-score) 

  (node-add-afunc
    root-node 
    (instr-saw 0.25 (env [0.0 220 0.1 200 0.0001 220 0.1 4000]) 0.0))


  (def my-score2
    (let [num-notes 10] 
      (node-events root-node 
                   (map #(event instr-square (* % 0.5)  
                                (/ 0.75 (+ 1 %)) 
                                (* 65 (+ 1 %)) 
                                (- (* 2 (/ % (- num-notes 1)))  1)) 
                        (range num-notes)))))

  (add-events my-score2) 

  (node-add-afunc
    root-node 
    (instr-square 0.25 (env [0.0 220 0.05 64 0.4 64]) 0.0))

  (stop-engine)


  )

