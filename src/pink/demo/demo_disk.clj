(ns pink.demo.demo-disk
  (:require [pink.engine :as eng]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             [pink.util :refer [mul]]
             [pink.oscillators :refer [oscil3 sine-table]]
             [pink.envelopes :refer [env]]
             )
  (:import [java.io File]))


(defn table-synth-cubic [freq]
  (println "Cubic...")
  (mul
     (oscil3 0.05 freq sine-table)
     (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])))


(comment

  (def e (eng/engine-create :nchnls 2))

  (def num-notes 5)
  (let [eng-events 
        (engine-events e
                       (map #(event horn (* % 0.5)  
                                    (/ 0.75 (+ 1 %)) 
                                    (* 220 (+ 1 %)) 
                                    (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]
      (eng/engine-add-afunc e (event-list-processor eng-events))) 

  (let [eng-events 
        (engine-events e
                       (map #(event horn-stopped (+ 5 (* % 0.5))  
                                    ;(/ 0.5 (+ 1 %)) 
                                    0.5
                                    (* 220 (+ 1 %)) 
                                    (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]
      (eng/engine-add-afunc e (event-list-processor eng-events)))

(let [eng-events 
        (engine-events e
                       (map #(event horn-muted (+ 10 (* % 0.5))  
                                    ;(/ 0.5 (+ 1 %)) 
                                    0.5
                                    (* 220 (+ 1 %)) 
                                    (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]
      (eng/engine-add-afunc e (event-list-processor eng-events)))


  (let [eng-events (engine-events e
                       (map #(event table-synth-cubic (+ 15 (* % 0.5)) 
                                    (* 220 (+ 1%))) (range num-notes)))]

      (eng/engine-add-afunc e (event-list-processor eng-events))
    
    )


  (eng/engine->disk e (str (System/getProperty "user.home") 
                           File/separator "test.wav"))

  (eng/engine-clear e)
  (eng/engine-kill-all)


  )

