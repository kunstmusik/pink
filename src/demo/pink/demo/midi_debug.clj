(ns pink.demo.midi-debug
  (:require [pink.io.midi :refer :all]))

(comment 
  (list-midi-devices)
  (list-midi-input-devices)

  (midi-device-debug "nanoKONTROL SLIDER/KNOB")

  (midi-device-debug "nanoKEY KEYBOARD")
  (midi-device-debug "MPKmini2")
  
  )
