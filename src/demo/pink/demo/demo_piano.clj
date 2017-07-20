(ns pink.demo.demo-piano
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.instruments.piano :refer :all]
             [pink.util :refer [mul try-func hold-until with-duration]]
             [pink.filters :refer :all]
             [pink.node :refer :all]
             [pink.space :refer :all]
             [pink.config :refer :all]
             [pink.envelopes :refer [env]]
             ))

(defn instr 
  [amp key-num]
  (->
    (piano :duration *duration* :keynum key-num :amp amp)
    ;(mul (hold-until 0.5 1.0 (env [0.0 1.0 0.1 0.0])))
    (pan 0.0)
    ))


(comment
  
  (start-engine)

  (add-audio-events
    (i instr 0.0 4.0 0.25 62))
  (add-audio-events
    (i instr 0.0 1.0 0.25 60)
    (i instr 0.5 1.0 0.25 64))

  (add-afunc (with-duration 4 (instr 0.5 60)))
  (add-afunc (with-duration 4 (instr 0.5 63)))

  (doseq [x (range 25)]
    (add-audio-events (i instr (* x 0.25) 1.0 0.25 (+ 60 x))))

  (doseq [x (range 25)]
    (add-audio-events (i instr (* x 0.25) 1.0 0.25 (+ 67 x))))

  (doseq [x (range 13)]
    (add-audio-events (i instr (* x 0.5) 2.0 0.25 (+ 40 x))))

  (doseq [x (range 1000)]
    (add-audio-events (i instr (* x 0.125) 2.0 
                         (* (/ 1.0 (inc (mod x 48))) 0.25) 
                         (+ 48 (mod x 48)))))

  (doseq [x (range 1000)]
    (add-audio-events (i instr (* x 0.2) 2.0 
                         (* (/ 1.0 (inc (mod x 48))) 0.25) 
                         (+ 48 (mod x 48)))))

  (add-audio-events
    (i instr 0.0 0.4 0.25 62)
    (i instr 0.5 0.4 0.25 65))


  )
