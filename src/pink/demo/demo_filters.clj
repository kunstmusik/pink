(ns pink.demo.demo-filters
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.oscillators :refer [blit-saw blit-square]]
             [pink.envelopes :refer [env xar]]
             [pink.util :refer [mul sum let-s]]
             [pink.noise :refer :all]
             [pink.filters :refer :all]
             [pink.delays :refer [adelay]]
             [pink.config :refer :all]
             ))

(defn test-filter
  [filterfn & args]
  (pan (mul 0.5 (apply filterfn (white-noise) args))
       0.0))

(comment

  (def score 
    (events test-filter 
            [0.0 butterlp (env [0.0 20 5 20000])]
            [5.0 butterhp (env [0.0 20 5 20000])]
            [10.0 butterbp (env [0.0 20 5 20000]) 100]
            [15.0 butterbr (env [0.0 20 5 20000]) 1000]
            ))

  (add-audio-events score)

  (start-engine)

  ;; Individual tests

  (add-audio-events 
    (event test-filter 0.0 butterlp (env [0.0 20 10 20000])))

  (add-audio-events 
    (event test-filter 0.0 butterhp (env [0.0 20 5 20000])))

  (add-audio-events 
    (event test-filter 0.0 butterbp (env [0.0 20 5 20000]) 100 ))

  (add-audio-events 
    (event test-filter 0.0 butterbr (env [0.0 20 5 20000]) 1000 ))

  (add-audio-events 
    (event test-filter 0.0 butterbr (env [0.0 20 5 20000]) 100 ))

  (add-audio-events 
    (event test-filter 0.0 moogladder (env [0.0 20 10 20000]) 0.1 ))
  

  (doseq [_ (range 5)] 
    (add-afunc
      (let [pch (+ 60 (rand-int 400))] 
        (let-s [;ampenv (xar 0.025 1.5)
                ampenv (env [0.0 0.0 0.025 1.0 0.025 0.9 1.0 0.9 2.0 0.0])
                cutenv (env [0.0 (* 6.0 pch) 0.025 (* 3.0 pch) 3.025 (* 3.0 pch)])
                ] 
          (pan (mul 0.5 ampenv
                    (moogladder (sum (mul 0.9 (blit-saw pch)) 
                                     (mul 0.2 (blit-saw (* pch 1.5)))) 
                                (sum (* pch 3) (mul (* pch 4) ampenv)) 
                                ;cutenv
                                ;(* pch 5)
                                0.85 

                                ))
               (- 1 (/ (rand-int 200) 100.0))))))) 

  ;(add-afunc (mul 0.5 (butterlp (white-noise) (env [0.0 20 5 20000]))))


  )
