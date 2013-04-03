(ns #^{:author "Steven Yi"
       :doc "Oscillator Functions"}
  audio-seq.oscillators
  (:use audio-seq.util)
  (:use [audio-seq.engine :only (*sr* )]))

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
