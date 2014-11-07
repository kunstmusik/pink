(ns pink.demo.demo-duration
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.oscillators :refer [blit-saw blit-square]]
             [pink.envelopes :refer [adsr xadsr]]
             [pink.util :refer :all]
             [pink.node :refer :all]
             [pink.filters :refer [tone butterlp]]
             [pink.delays :refer [adelay]]
             [pink.config :refer [*duration*]]
             ))

(defn instr-saw
  [amp freq loc]
  (let-s [e (adsr 0.03 0.01 0.9 0.5)] 
    (pan 
      (mul e
           (butterlp (blit-saw freq) 
                 (sum 100 (mul e 400))))
      loc)))


(defn instr-saw-xadsr
  [amp freq loc]
  (let-s [e (xadsr 0.5 0.5 0.5 0.5)] 
    (pan 
      (mul e
           (butterlp (blit-saw freq) 
                 (sum 100 (mul e 400))))
      loc)))

(comment

  (start-engine)

  (add-afunc (instr-saw 0.5 440.0 0.0))

  (add-afunc
    (binding [*duration* 2.0]
      (instr-saw 0.5 440.0 0.0)))

  (add-afunc
    (with-duration 0.035
      (instr-saw 0.5 440.0 0.0)))


  (add-afunc (instr-saw-xadsr 0.5 440.0 0.0))

  (add-afunc
    (binding [*duration* 2.0]
      (instr-saw-xadsr 0.5 440.0 0.0)))

  (add-afunc
    (with-duration 2.0 
      (instr-saw-xadsr 0.5 440.0 0.0)))

  (stop-engine)


  )

