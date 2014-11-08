(ns pink.demo.demo-feedback
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.node :refer :all]
             [pink.space :refer [pan]] 
             [pink.oscillators :refer [blit-saw blit-square]]
             [pink.envelopes :refer :all]
             [pink.util :refer :all]
             [pink.noise :refer :all]
             [pink.filters :refer :all]
             [pink.delays :refer :all]
             [pink.config :refer :all]
             ))


(defn ping-pong-delay
  "Creates a stereo ping-pong delay given a mono audio function."
  [afn left-delay-time left-amp-mod
   right-delay-time right-amp-mod]
  (let [^"[[D" out (create-buffers 2) 
        ain (shared afn) 
        lfeedback (create-buffer 0.0)
        rfeedback (create-buffer 0.0)
        ldelay (feedback-write  
                 (adelay (sum ain 
                              (mul left-amp-mod (feedback-read rfeedback)))
                         left-delay-time)
                 lfeedback)
        rdelay (feedback-write 
                 (adelay (sum ain
                              (mul right-amp-mod (feedback-read lfeedback))) 
                         right-delay-time) 
                 rfeedback )] 
    (fn []
      (let [aleft (ldelay)
            aright (rdelay)]
          (when (and aleft aright)
            (aset out 0 aleft) 
            (aset out 1 aright) 
            out)))))

(defn instr-saw
  [amp freq]
  (let-s [e (adsr 0.01 1.0 0.0 0.0)] 
    (mul e
         (moogladder (blit-saw freq) 
                   (sum 800 (mul e 800))
                   0.45
                   ))))

(comment

  (def saw-node (create-node))
  (def saw-processor (shared (node-processor saw-node)))

  (add-afunc (pan saw-processor 0.0))
  (add-afunc (ping-pong-delay saw-processor 
                              0.57 0.9 0.43 0.8))

  (start-engine)

  (node-add-func saw-node (instr-saw 0.25 (+ 200 (* 200 (rand)))))  


  )
