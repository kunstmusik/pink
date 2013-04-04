(ns audio-seq.oscillators
  "Oscillator Functions"
  (:require [audio-seq.engine :refer [*sr*]]
            [audio-seq.util :refer [create-buffer fill map-d]]))

(def ^:const PI Math/PI)

(defn- ^double dec-if [^double a] (if (> a 1) (dec a) a))

(defn phasor [^double freq ^double phase]
  (let [phase-incr ^double (/ freq  *sr*)
        cur-phase (double-array 1 phase)
        out (create-buffer)]
      (fn ^doubles [] 
        (fill out cur-phase #(dec-if (+ phase-incr ^double %))))))

(defn sine [^double freq ^double phase]
  (let [phsr (phasor freq phase)
        out (create-buffer)]
    (fn ^doubles []
      (map-d #(Math/sin (* 2.0 PI ^double %)) (phsr) out))))
