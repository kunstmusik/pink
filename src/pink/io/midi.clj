(ns ^{ :doc "Functions for handling MIDI event and controller input
            
            Pink's design  uses a virtual device system so that projects 
            can be written to depend on the virtual device, and the
            real device can be configured per-system using a .pinkrc
            file. This allows both swapping in and out of hardware as 
            well as creating mock hardware devices.
            
            Conventions are to use the following for virtual hardware
            names:

            * \"keyboard x\" - number of keyboard
            * \"knobs/sliders x\" - number of knobs/slider device

            Note, a hardware device can map to multiple virtual devices."

      :author "Steven Yi"}
  pink.io.midi
  (:import [javax.sound.midi MidiSystem MidiDevice$Info]))


;; functions for listing registered MIDI devices

(defn list-midi-devices []
  (let [infos (MidiSystem/getMidiDeviceInfo)]
    (loop [i 0
           [^MidiDevice$Info x & xs] infos 
           retval []]
      (if x 
        (recur (unchecked-inc i) 
               xs 
               (conj retval {:name (.getName x) 
                             :description (.getDescription x) 
                             :device-info x}))
        retval))))

(defn midi-input-device?
  [d] 
  (not (zero? (.getMaxTransmitters (MidiSystem/getMidiDevice (:device-info d))))))

(defn midi-output-device?
  [d] 
  (not (zero? (.getMaxReceivers (MidiSystem/getMidiDevice (:device-info d))))))

(defn list-midi-input-devices []
 (filter midi-input-device? (list-midi-devices)))


(defn list-midi-output-devices []
 (filter midi-output-device?  (list-midi-devices)))


;; Pink MIDI Manager

(defn create-midi-manager []
  (atom {}))

(defn add-virtual-device
  [midi-manager device-name] 
  (swap! midi-manager assoc device-name 
         {:name device-name
          :keys (boolean-array 128 false)
          :event-processors (into-array (for [i (range 16)] (atom [])))
          :cc-processors (into-array (for [i (range 128)] (atom [])))
          }))

(defn list-devices 
  [midi-manager]
  @midi-manager)

(comment
  (let [f (create-midi-manager)]
    (add-virtual-device f "slider/knobs 1") 
    (add-virtual-device f "keyboard 1") 
    (println (list-devices f))))

;; Binding

(defn bind-device 
  [hardware-id virtual-device-name]
  )

(defn add-midi-cc-processor 
  [virtual-device channel cc-num mapping-func]
  
  )

(defn add-midi-event-processor
  [virtual-device channel midi-event-func]

  )
