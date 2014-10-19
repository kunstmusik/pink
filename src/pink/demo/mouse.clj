(ns pink.demo.mouse
  (:require [pink.io.mouse :refer :all]
            [pink.simple :refer :all]
            [pink.filters :refer :all]
            [pink.space :refer :all]
            [pink.envelopes :refer :all]
            [pink.oscillators :refer :all]
            [pink.util :refer :all]
            ))

;; create instrument


(defn instr-saw
  [amp freq]
  (pan 
    (mul (tone (blit-saw freq) 1000) amp)
    0.5))

(comment

  (start-engine)

  (add-afunc 
    (instr-saw (mul (sub 1 (mul (port (mouse-y) 0.1) (/ 1 (get-screen-height)))))
               (sum 200 (mul (port (mouse-x) 0.1) 2000 (/ 1 (get-screen-width))))))
  
  )
