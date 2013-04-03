(ns #^{:author "Steven Yi"
       :doc "Protocols for Audio Engine"}
  audio-seq.protocols
  (:use audio-seq.util)
  (:use [audio-seq.engine :only (*sr* )]))

(defprotocol AudioBlock
  (get-buffer [this] "Returns an audio buffer (double[]) full with audio"))

(defprotocol EventBlock
  (process-events [this] "process all current events")) 
