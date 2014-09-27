(ns pink.demo.midi
  (:require [pink.engine :refer :all] 
            [pink.io.midi :refer :all]
            [pink.config :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer [oscili]]
            [pink.envelopes :refer [env]]
            [pink.filters :refer [port]]
            [pink.util :refer [mul try-func create-buffer]])
  (:import [javax.sound.midi MidiSystem Transmitter Receiver MidiMessage
            ShortMessage ]
           [java.util Arrays]))


(def midim (create-midi-manager))
(def sliders (add-virtual-device midim "slider/knobs 1")) 
(def keyboard (add-virtual-device midim "keyboard 1")) 

(bind-device midim "nanoKONTROL SLIDER/KNOB" "slider/knobs 1")

;(midi-device-debug "nanoKONTROL SLIDER/KNOB")

(def get-cc (partial get-midi-cc-atom sliders 0))

;(defn cc-trigger []
;  (println "hello."))

;(add-watch (get-cc 60) "trigger1"
;           (midi-cc-trigger cc-trigger))

(defn atom-reader
  [source-atom]
  (let [out ^doubles (create-buffer)
        cur-val (atom @source-atom)]
    (fn []
      (when (not (= @cur-val @source-atom))
        (let [])
        (Arrays/fill out ^double (reset! cur-val @source-atom)))
      out
      )))


(defn rescale-midi 
  [source-fn target-mn target-mx]
  (let [out ^doubles (create-buffer) 
        target-range (- target-mx target-mn)]
    (fn [] 
      (when-let [buf ^doubles (source-fn)]
        (loop [i 0]
          (if (< i *buffer-size*)
            (do
              (aset out i
                  (+ target-mn (* target-range 
                                  (/ (aget buf i) 127))))
              (recur (unchecked-inc i)))
            out))))))

(def e (engine-create :nchnls 2))
(engine-start e)

(defn create-osc [space freq ampcc freqcc]
  (pan 
    (oscili (port (rescale-midi (atom-reader (get-cc ampcc)) 0.0 0.1) 
                  0.05)
            (port (rescale-midi (atom-reader (get-cc freqcc)) freq (* 2 freq)) 
                  0.05))

    space)
  )

(defn scale-space [v low high]
  (+ low (* v (- high low))))

(def play (partial engine-add-afunc e))

(print (map play 
     (for [x (range 1 10)]
           (let [f (+ 200 (* x 100))] 
             (create-osc (scale-space (/ (- x 1) 8.0) -0.5 0.5) 
                         f x (+ x 10))))))

;(def osc1 (create-osc 0.25 400 1 11))
;(def osc2 (create-osc -0.25 300 2 12))

;(engine-add-afunc e osc1)
;(engine-add-afunc e osc2)

(comment
  (engine-kill-all)
  )
