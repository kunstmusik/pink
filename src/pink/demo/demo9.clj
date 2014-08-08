;; Test of Events 

(ns pink.demo.demo9
  (:require [pink.audio.engine :as eng]
             [pink.event :refer :all] 
             [pink.instruments.horn :refer :all]
             ))

(comment

  (def e (eng/engine-create))
  (eng/engine-start e)

  (let [eng-events (engine-events e
                       (map #(event horn (* % 0.5)  (/ 0.5 %) (* 220 %)) 
                            (range 1 6)))]

      (eng/engine-add-afunc e (eng-events-runner eng-events))
    
    ) 

  (eng/engine-stop e)
  (eng/engine-clear e)
  (eng/engine-kill-all)

  (let [e (eng/engine-create)
        eng-events 
        (engine-events e
                       (event table-synth 0.0 440.0) 
                       (event table-synth 0.0 550.0) 
                       ;(map #(event table-synth-interp 0.25 (* 110 %)) (range 1 36)) 

                       (event table-synth-interp 1.0 440.0) 
                       ;(event table-synth-interp 1.0 550.0)

                       (event table-synth-cubic 2.0 440.0) 
                       (event table-synth-cubic 2.0 550.0)

                       ;(event table-synth-interp 1.0 440.0) 
                       ;(event table-synth-interp 1.0 550.0)
                       )
        ]
    
      (eng/engine-start e)
      (eng/engine-add-afunc e (eng-events-runner eng-events))

      (Thread/sleep 3000)
      (eng/engine-stop e)
      (eng/engine-clear e))


  )

