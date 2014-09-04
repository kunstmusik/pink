(ns pink.noise
  "Noise audio-functions."
  (:require [pink.util :refer [create-buffer fill]]))

(defn white-noise  []
  (let  [out  (create-buffer)
         state (double-array 1 0)] 
    (fn  []
      (fill out state 
        (fn [a] (-  (* 2  (Math/random) 1)))))))

