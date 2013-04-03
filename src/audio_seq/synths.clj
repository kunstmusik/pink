(ns #^{:author "Steven Yi"
       :doc "Synths encapsulate polyphonic audio entities"}
  audio-seq.synths
  (:use audio-seq.util)
  (:use [audio-seq.engine :only (*sr* )]))

(defn synth-create []
  {:name nil
  :audiofunc nil
  :eventlist nil
  })


