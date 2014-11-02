(ns pink.demo.demo-audio-file-load
 (:require [pink.engine :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.io.sound-file :refer [load-table]]
             [pink.oscillators :refer [oscil3]]
             [pink.envelopes :refer [env]]
             [pink.util :refer [mul]]
             [pink.node :refer :all]
             [pink.config :refer [*sr*]]
             ))

;(def wave 
;  (load-table "/Users/stevenyi/work/csound/samples/akwf/AKWF_bw_sawbright/AKWF_bsaw_0001.wav"))

(def wave (load-table "/Users/stevenyi/work/csound/samples/salamanderDrumkit/OH/kick_OH_FF_1.wav"))

(def duration
  (let [d ^doubles (aget (:data wave) 0)]
    (/ (alength d) (double *sr*))))

(defn instr-waveform
  [amp freq loc]
  (pan 
    (mul (env [0 0 0.02 amp (- duration 0.07) amp 0.05 0.0])
         (oscil3 1.0 (/ 1.0 duration) (aget (:data wave) 0)))
    loc))


(comment

  (def e (engine-create :nchnls 2))
  (engine-start e)

  (def root-node (create-node :channels 2))
  (engine-add-afunc e (node-processor root-node))

  (def my-score 
    (let [num-notes 10] 
      (node-events root-node 
                   (map #(event instr-waveform (* % 0.5)  
                                (/ 0.75 (+ 1 %)) 
                                (* 220 (+ 1 %)) 
                                (- (* 2 (/ % (- num-notes 1)))  1)) 
                        (range num-notes)))))

  (engine-add-events e my-score) 

  (node-add-afunc
    root-node 
    (instr-waveform 0.25 (env [0.0 220 0.1 4000 0.0001 220 0.1 4000]) 0.0))

  (engine-stop e)
  (engine-clear e)
  (engine-kill-all)


  )

