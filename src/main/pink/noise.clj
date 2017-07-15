(ns pink.noise
  "Noise audio-functions."
  (:require 
    [pink.config :refer [*sr*]]
    [pink.util :refer [create-buffer generator 
                       gen-recur not== arg]]))

;; Ensure unchecked math used for this namespace
(set! *unchecked-math* :warn-on-boxed)

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


(defn dust
  "Generate random impulses from 0 to +1. 

  density - average number of impulses per second
  mul - amplitude multiplier
  
  Based on Dust Ugen from SuperCollider 3."
  ([density] (dust density 1.0))
  ([density mul] 
    (let [out (create-buffer)
          dfn (arg density)
          mfn (arg mul)
          sr (double *sr*)
          onedsr (/ 1.0 sr)]
      (generator
        [last-thresh 0 last-d 0]
        [d dfn, m mfn]
        (let [thresh 
              (if (not== d last-d)
                (* d onedsr) 
                last-thresh)
              scale (if (> thresh 0.0) (/ 1.0 thresh) 0.0)
              z (Math/random)
              v (if (< z thresh)
                  (* m (* z scale)) 
                  0.0)]
          (aset out int-indx v)
          (gen-recur thresh d))
        (yield out))))) 

(defn dust2
  "Generate random impulses from -1 to +1. 

  density - average number of impulses per second
  mul - amplitude multiplier
  
  Based on Dust2 Ugen from SuperCollider 3."
  
  ([density] (dust2 density 1.0))
  ([density mul] 
    (let [out (create-buffer)
          dfn (arg density)
          mfn (arg mul)
          sr (double *sr*)
          onedsr (/ 1.0 sr)]
      (generator
        [last-thresh 0 last-d 0]
        [d dfn, m mfn]
        (let [thresh 
              (if (not== d last-d)
                (* d onedsr) 
                last-thresh)
              scale (if (> thresh 0.0) (/ 2.0 thresh) 0.0)
              z (Math/random)
              v (if (< z thresh)
                  (* m (- (* z scale) 1.0)) 
                  0.0)]
          (aset out int-indx v)
          (gen-recur thresh d))
        (yield out))))) 
