(ns pink.noise
  "Noise audio-functions."
  (:require [pink.util :refer [create-buffer generator gen-recur]]))

(defn white-noise  
  "Create white-noise generator."
  []
  (let [out ^doubles (create-buffer)] 
    (generator
      [] []
      (let [v (- (* 2 (Math/random)) 1)]
        (aset out int-indx v)
        (gen-recur))
      (yield out))))



