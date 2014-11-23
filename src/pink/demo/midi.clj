(ns pink.demo.midi
  (:require [pink.simple :refer :all] 
            [pink.io.midi :refer :all]
            [pink.config :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer [oscili]]
            [pink.envelopes :refer [env]]
            [pink.filters :refer [port]]
            [pink.util :refer [mul try-func create-buffer generator]])
  (:import [java.util Arrays]))


(comment

  (def midim (create-midi-manager))
  (def sliders (add-virtual-device midim "slider/knobs 1")) 
  (def keyboard (add-virtual-device midim "keyboard 1")) 

  (bind-device midim "nanoKONTROL SLIDER/KNOB" "slider/knobs 1")

  ;(midi-device-debug "nanoKONTROL SLIDER/KNOB")

  (def get-cc (partial get-midi-cc-atom sliders 0))


  (defn midi-atom-reader
    [source-atom ^double target-mn ^double target-mx]
    (let [out ^doubles (create-buffer)
          cur-val (atom @source-atom)
          target-range (- target-mx target-mn)]
      (fn []
        (let [v @source-atom] 
          (when (not (= @cur-val v))
            (let [new-v (+ target-mn (* target-range (/ (double v) 127.0)))]
              (reset! cur-val v) 
              (Arrays/fill out new-v))))
        out
        )))

  (start-engine)

  (defn create-osc [space freq ampcc freqcc]
    (pan 
      (oscili (port (midi-atom-reader (get-cc ampcc) 0.0 0.1) 0.05)
              (port (midi-atom-reader (get-cc freqcc) freq (* 2 freq)) 0.05))
      space))

  (defn scale-space [v low high]
    (+ low (* v (- high low))))

  (doseq [x (range 1 10)]
    (let [f (+ 200 (* x 100))] 
      (add-afunc 
        (create-osc (scale-space (/ (- x 1) 8.0) -0.5 0.5) 
                    f x (+ x 10)))))
  )
