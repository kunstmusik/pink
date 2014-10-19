(ns pink.noise
  "Noise audio-functions."
  (:require [pink.util :refer [create-buffer generator]]))

(defn white-noise  
  "Create white-noise generator."
  []
  (let [out ^doubles (create-buffer)] 
    (generator
      []
      []
      (let [v (-  (* 2  (Math/random) 1))]
        (aset out indx v)
        (recur (unchecked-inc-int indx)))
      (yield out))))



