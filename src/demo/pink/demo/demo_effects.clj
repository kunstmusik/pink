(ns pink.demo.demo-effects
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.oscillators :refer :all]
             [pink.effects.chorus :refer [chorus]]
             [pink.effects.ringmod :refer [ringmod]]
             [pink.effects.reverb :refer [freeverb]]
             [pink.envelopes :refer [env xar adsr]]
             [pink.util :refer [mul sum let-s with-duration]]
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

(defn instr-triangle
  [amp freq loc]
  (let-s [e (if (fn? amp) 
              amp
              (mul amp (env [0.0 0.0 0.1 1.0 3.0 1.0 0.1 0.0])))] 
    (->
      (blit-triangle freq) 
      ;(butterlp (sum 100 (mul e 400)))
      (mul e) 
      (pan loc))))

(defn vox-humana 
  [amp ^double freq ^double loc]
  (let  [pulse-freq (mul freq (sum 1.0004 (lfo 0.013 3.5 :triangle)))
         pulse-width (sum 0.625 (lfo 0.125 5.72 :triangle))
         saw-freq (mul freq (sum 1 (lfo 0.021 5.04 :triangle)))
         key-follow (+ 1 (Math/exp (/ (- freq 50.0) 10000.0))) ] 
    (let-s [e (if (fn? amp) 
                amp
                (mul amp (env [0.0 0.0 0.1 1.0 3.0 1.0 0.1 0.0])))] 
      (->
        (sum (blit-saw saw-freq) 
          (blit-pulse pulse-freq pulse-width)) 

        (butterlp (* key-follow 1986))
        (mul e 0.5) 
        (pan loc)))))


(defn sawz
  [^double dur freq freq2 amp]
  (with-duration dur 
    (-> 
      (sum (blit-saw freq)
           (blit-saw freq2))
      (moogladder 2000 0.3)
      (mul (adsr 0.01 0.01 0.9 0.3) amp)
      (pan 0.0) 
      )))

;(def a (instr-saw 0.1 440 0.0))
;(def b (blit-saw 440))
;(require '[no.disassemble :refer :all])
;(println (disassemble b))
;(require '[clojure.pprint :refer [pprint]])
;(pprint (a))

(comment

  (start-engine)

  (def root-node (create-node :channels 2))
  (add-afunc (chorus (node-processor root-node) 0.8))

  (def reverb-node (create-node :channels 2))
  (add-afunc (freeverb (node-processor reverb-node) 0.90 0.5))

  ;; chorus
  (node-add-func
    root-node
    (with-duration 4.0
      (-> 
        (sum (blit-saw 200)
             (blit-saw 300))
        (moogladder 600 0.2)
        (mul (adsr 0.4 0.1 0.9 2.0) 0.5)
        (pan 0.05) 
        )))

  ;; circuit modeled ringmod
  (add-afunc
    (with-duration 4.0
      (-> 
        (sum (blit-saw 200)
             (blit-saw 300))
        (ringmod (sine 400) 1.1)
        (moogladder 1200 0.2)
        (mul (adsr 0.4 0.1 0.9 2.0) 0.5)
        (pan 0.05) 
        )))

  ;; digital ringmod using mul
  (add-afunc
    (with-duration 4.0
      (-> 
        (sum (blit-saw 200)
             (blit-saw 300))
        (mul (sine 400))
        (moogladder 1200 0.2)
        (mul (adsr 0.4 0.1 0.9 2.0) 0.5)
        (pan 0.05) 
        )))

  ;; freeverb

  (node-add-func
    reverb-node
    (instr-saw 1.0 600 0.2))

  (node-add-func
    reverb-node
    (sawz 2.0 700 1200 0.2))

  (node-add-func
    reverb-node
    (sawz 2.0 100 150 0.2))

  (node-add-func
    reverb-node
    (sawz 1.0 60 90 0.251))

  (add-afunc
    (sawz 2.0 100 150 0.2))

  ;;
  (add-afunc
    (with-duration 8.0
      (vox-humana (mul 0.5 (adsr 0.453 0.0 1.0 2.242)) 440 0.0)))

  (node-add-func
    root-node
    (with-duration 8.0
      (vox-humana (mul 0.5 (adsr 0.453 0.0 1.0 2.242)) 440 0.0)))

  (add-afunc
    (with-duration 8.0
      (vox-humana (mul 0.5 (adsr 0.453 0.0 1.0 2.242)) 880 0.0)))

  (node-add-func
    root-node
    (with-duration 8.0
      (vox-humana (mul 0.5 (adsr 0.453 0.0 1.0 2.242)) 880 0.0)))

  (node-add-func
    root-node
    (with-duration 8.0
      (vox-humana (mul 0.5 (adsr 0.453 0.0 1.0 2.242)) 1320 0.0)))

  (def my-score3
    (let [num-notes 10] 
      (node-events root-node 
                   (map #(event instr-triangle (* % 0.5)  
                                (/ 0.75 (+ 1 %)) 
                                (* 65 (+ 1 %)) 
                                (- (* 2 (/ % (- num-notes 1)))  1)) 
                        (range num-notes)))))



  (add-events my-score3) 

  (stop-engine)


  )

