(ns pink.demo.midi-keys
  (:require [pink.simple :refer :all] 
            [pink.io.midi :refer :all]
            [pink.config :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer [blit-saw]]
            [pink.envelopes :refer [env xar]]
            [pink.filters :refer [port butterlp]]
            [pink.util :refer [mul try-func create-buffer generator]])
  (:import [javax.sound.midi MidiSystem Transmitter Receiver MidiMessage
            ShortMessage ]
           [java.util Arrays]
           [clojure.lang IFn]))


(def midim (create-midi-manager))
(def keyboard (add-virtual-device midim "keyboard 1")) 

(defn saw
  [freq amp]
  (mul amp (blit-saw freq)))

(bind-device midim "nanoKEY KEYBOARD" "keyboard 1")

(bind-key-func
  keyboard 0
  (let [active (make-array IFn 128)] 
    (fn [cmd note-num velocity]
      (println ">> " cmd " " note-num " " velocity)
      (condp = cmd
         ShortMessage/NOTE_ON
         (let [afn (saw (midi->freq note-num) (/ velocity 127))]
           (aset active note-num afn)
           (add-afunc afn))

         ShortMessage/NOTE_OFF
        (when-let [afn (aget active note-num)]
           (remove-afunc afn)
           (aset active note-num afn)))
        )))



(start-engine)

