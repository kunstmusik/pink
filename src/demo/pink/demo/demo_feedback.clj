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
                 (fdelay (sum ain 
                              (mul left-amp-mod (feedback-read rfeedback)))
                         left-delay-time)
                 lfeedback)
        rdelay (feedback-write 
                 (fdelay (sum ain
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
  (let-s [e (adsr 0.5 0.0 1.0 4.0)] 
    (-> (blit-saw freq)
        (moogladder (sum 800 (mul e 4000)) 0.5)
        (mul e amp)) 
    ))

(comment
  (defn get-samples 
    ^doubles [afn ^long num-samples]
    (let [out ^doubles (double-array num-samples)]
      (loop [^doubles vs (afn) index 0 buffer 0]
        (let [q (quot index (long *buffer-size*))
              r (rem index (long *buffer-size*))] 
          (if (< index num-samples)
            (if (> q buffer)
              (recur (afn) index q)
              (do 
                (aset out index (* 16384 (aget vs r)))
                (recur vs (inc index) buffer)))
            out)))))

  (def samps (get-samples (with-duration 10.0 (instr-saw 0.5 440)) 40000))

  (spectrogram samps))

(comment

  (def saw-node (audio-node))
  (def saw-processor (shared saw-node))

  (add-afunc (pan saw-processor 0.0))
  (add-afunc (ping-pong-delay saw-processor 
                              0.57 0.9 0.43 0.8))

  (start-engine)

  (node-add-func saw-node (with-duration 5.0 (instr-saw 0.25 (env [0.0 40 5.0 1000]))))  
  (node-add-func saw-node (with-duration 5.0 (instr-saw 0.25 (env [0.0 80 5.0 2000]))))  
  (node-add-func saw-node (with-duration 5.0 (instr-saw 0.25 (env [0.0 120 5.0 3000]))))  

  (clear-engine)
  (stop-engine)

  )
