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

  (add-engine-events score)

  (start-engine)

  ;; Individual tests

  (add-engine-events 
    (event test-filter 0.0 butterlp (env [0.0 20 10 20000])))

  (add-engine-events 
    (event test-filter 0.0 butterhp (env [0.0 20 5 20000])))

  (add-engine-events 
    (event test-filter 0.0 butterbp (env [0.0 20 5 20000]) 100 ))

  (add-engine-events 
    (event test-filter 0.0 butterbr (env [0.0 20 5 20000]) 1000 ))

  (add-engine-events 
    (event test-filter 0.0 butterbr (env [0.0 20 5 20000]) 100 ))

  (add-engine-events 
    (event test-filter 0.0 moogladder (env [0.0 20 10 20000]) 0.1 ))
  

  (add-afunc
    (let [pch (+ 220 (rand-int 880))] 
      (let-s [ampenv (xar 0.05 1.5)] 
        (pan (mul 0.5 ampenv
           (moogladder (sum (blit-saw pch) (blit-square (* pch 2))) 
                       (sum (* pch 4) (mul (* pch 8) ampenv)) 
                       0.15))
         0.0))))

  ;(add-afunc (mul 0.5 (butterlp (white-noise) (env [0.0 20 5 20000]))))


  )
