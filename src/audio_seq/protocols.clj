(ns audio-seq.protocols
  "Protocols for Audio Engine")

(defprotocol AudioBlock
  (get-buffer [this] "Returns an audio buffer (double[]) full with audio"))

(defprotocol EventBlock
  (process-events [this] "process all current events")) 
