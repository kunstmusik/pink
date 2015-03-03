(ns pink.demo.demo-filters
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.space :refer [pan]] 
             [pink.oscillators :refer :all]
             [pink.envelopes :refer [env xar adsr140]]
             [pink.util :refer [mul sum let-s with-duration]]
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

  (defn example [freq] 
    (let-s [ramp-env (env [0.0 0.0 5.0 1.0 10.0 0.0])
            e (adsr140 
                (sine2 (sum 3.0 (mul ramp-env 15.0))) 
                0 
                0.04 0.02 0.9 0.15)] 
      (-> 
        (mul e ramp-env 
             (sum (blit-saw freq)
                  (blit-saw (mul freq 1.002581))))
        (moogladder (sum 1000 (mul 500 6 e ramp-env)) 0.6)
        (pan 0.0)
        )))

  (add-afunc (example 660.0))
  (add-afunc (example 550.0))
  (add-afunc (example 880.0))
  (add-afunc (example 440.0))
  (add-afunc (example 1320.0))
  (add-afunc (example 1000.0))

  )
