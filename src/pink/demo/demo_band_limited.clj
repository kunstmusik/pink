(ns pink.demo.demo-band-limited
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.oscillators :refer [blit-saw blit-square]]
             [pink.envelopes :refer [env xar]]
             [pink.util :refer [mul sum let-s]]
             [pink.node :refer :all]
             [pink.filters :refer :all]
             [pink.delays :refer [adelay]]
             ))

(defn instr-saw
  [amp freq loc]
  (let-s [e (xar 0.01 1.0)] 
    (pan 
      (mul e
           (lpf18 (sum
                   (blit-saw freq)
                   (blit-saw (sum 0.873 freq))
                   (blit-saw (sum -0.95117 freq)))
                  (sum 4000 (mul e 2000)) 
                  0.6 0.1)
           ;(butterlp (blit-saw freq) 
           ;      (sum 100 (mul e 400)))
           
           )
      loc)))

(defn instr-square
  [amp freq loc]
  (let-s [e (xar 0.01 1.0)] 
    (pan 
      (mul e amp
           (butterlp (blit-square freq) 
                 (sum 100 (mul e 400))))
      loc)))

;(def a (instr-saw 0.1 440 0.0))
;(def b (blit-saw 440))
;(require '[no.disassemble :refer :all])
;(println (disassemble b))
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

  (node-add-func
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

  (node-add-func
    root-node 
    (instr-square 0.5 (env [0.0 200 0.05 40 0.4 40]) 0.0))

  (stop-engine)


  )

