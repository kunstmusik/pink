(ns audio-seq.oscillators2
  "Oscillator Functions"
  (:require [audio-seq.engine :refer [*sr*]]
            [audio-seq.util :refer [create-buffer fill map-d swapd! setd! getd arg]]  
            [hiphip.double :as dbl]
            [hiphip.array :as arr]
            ))

(def ^:const PI Math/PI)

(defn- ^double dec-if [^double a] (if (> a 1) (dec a) a))

(defn phasor [^double freq ^double phase]
  (let [phase-incr ^double (/ freq  *sr*)
        cur-phase (double-array 1 phase)
        out (create-buffer)
        ]
      (fn ^doubles [] 
        (fill out cur-phase #(dec-if (+ phase-incr ^double %))))))

(defn sine 
  ([^double freq]
   (sine freq 0.0))
  ([^double freq ^double phase]
   (let [phsr (phasor freq phase)
         out (create-buffer)]
     (fn ^doubles []
       (dbl/amap [x (phsr)] (Math/sin (* 2.0 PI x)))))))


(defn vphasor [freq phase]
  (let [out ^doubles (create-buffer)
        cur-phase (double-array 1 0)
        len (alength ^doubles out)
        lastindx (dec len)]
    (fn ^doubles [] 
      (let [f (freq)
            p (phase)]
        (when (and f p)
          (loop [cnt (unchecked-long 0)]
            (if (< cnt len)
              (let [incr ^double (/ (aget ^doubles f cnt) *sr*)
                    phs-adj (aget ^doubles p cnt)
                    phs-func (fn [^double c] ^double (+ c incr phs-adj))] 
                (aset out cnt (setd! cur-phase (dec-if (phs-func (getd cur-phase)))))
                (recur (unchecked-inc cnt)))
              out)))))))

(defn sine2 
  ([f]
   (sine2 f 0))
  ([f p]
   (let [phsr (vphasor (arg f) (arg p))
         out (create-buffer)]
     (fn ^doubles []
       (dbl/amap [x (phsr)] (Math/sin (* 2.0 PI x)) )))))
