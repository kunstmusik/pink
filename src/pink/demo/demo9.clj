(ns pink.demo.demo9
  (:require [pink.engine :refer :all]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             [pink.util :refer [mul]]
             [pink.oscillators :refer [oscil3 sine-table]]
             [pink.envelopes :refer [env]]
             ))


(defn table-synth-cubic [freq]
  (println "Cubic...")
  (mul
     (oscil3 0.05 freq sine-table)
     (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])))


(comment

  (def e (engine-create :nchnls 2))
  (engine-start e)

  (def num-notes 5)
  (let [eng-events 
        (audio-events e
                       (map #(event horn (* % 0.5)  
                                    (/ 0.75 (+ 1 %)) 
                                    (* 220 (+ 1 %)) 
                                    (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]
      (engine-add-events e eng-events)) 

  (let [eng-events 
        (audio-events e
                       (map #(event horn-stopped (* % 0.5)  
                                    ;(/ 0.5 (+ 1 %)) 
                                    0.5
                                    (* 220 (+ 1 %)) 
                                    (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]
    (engine-add-post-cfunc e 
      (event-list-processor (event-list eng-events))))

  (let [eng-events 
        (audio-events e
                       (map #(event horn-muted (* % 0.5)  
                                    ;(/ 0.5 (+ 1 %)) 
                                    0.5
                                    (* 220 (+ 1 %)) 
                                    (- (* 2 (/ % (- num-notes 1)))  1)) 
                            (range num-notes)))]
      (engine-add-post-cfunc e 
        (event-list-processor (event-list eng-events))))


  (let [eng-events (audio-events e
                       (map #(event table-synth-cubic (* % 0.5) (* 220 (+ 1%))) (range num-notes)))]
      (engine-add-post-cfunc e 
        (event-list-processor (event-list eng-events)))
    
    )
  (engine-stop e)
  (engine-clear e)
  (engine-kill-all)


  )

