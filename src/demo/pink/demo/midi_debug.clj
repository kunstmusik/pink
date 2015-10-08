(ns pink.demo.midi-debug
  (:require [pink.io.midi :as midi]))

(comment 
  (midi/list-devices)
  (midi/list-input-devices)

  (midi/device-debug "nanoKONTROL SLIDER/KNOB")

  (midi/device-debug "nanoKEY KEYBOARD")
  (midi/device-debug "MPKmini2")
  
  )
