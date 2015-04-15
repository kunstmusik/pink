(ns pink.effects.reverb
  (:require [pink.config :refer [*sr*]]
           [pink.util :refer :all])
  (:import [clojure.lang IFn$DD]))

(def ^:private ^{:tag 'double} ORIG-SR 44100.0)
(def ^:private ^{:tag 'double} ALLPASS-FEEDBACK 0.5)
(def ^:private ^{:tag 'long} DEFAULT-STEREO-SPREAD 23)
(def ^:private ^{:tag 'double} FIXED-GAIN 0.015)
(def ^:private COMB-TUNING 
  [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617])
(def ^:private ALLPASS-TUNING
  [556, 441, 341, 225])

(defn- adjust-tuning 
  "Returns adjusted tuning values as originals were
  defined in terms of sr=44100."
  ^long [^long tuning]
  (long (/ (* tuning (double *sr*)) ORIG-SR )))

(defn- frvb-comb
  ^IFn$DD [^long del-time ^double feedback ^double damp] 
  (let [^doubles buffer (double-array del-time)
        ^doubles last-filt (double-array 1)
        ^ints indx (int-array 1 0)
        damp2 (- 1.0 damp)]
    ;(println "Comb Tuning: " del-time " " feedback " " damp " " damp2)
    (fn ^double [^double input]
      (let [i (aget indx 0)
            output (aget buffer i)
            new-i (int (mod (inc i) del-time))
            filt-store (+ (* output damp2) (* (aget last-filt 0) damp))]
        (aset buffer i (+ input (* filt-store feedback))) 
        (aset last-filt 0 filt-store)
        (aset indx 0 new-i)
        output))))

(defn- frvb-allpass
  ^IFn$DD [^long del-time ^double feedback] 
  (let [^doubles buffer (double-array del-time)
        ^ints indx (int-array 1 0)]
    ;(println "Allpass Tuning: " del-time " " feedback)
    (fn ^double [^double input]
      (let [i (aget indx 0)
            bufout (aget buffer i)
            new-i (int (mod (inc i) del-time))
            output (- bufout input)]
        (aset buffer i (+ input (* bufout feedback))) 
        (aset indx 0 new-i)
        output))))

(defn- par
  ^double [^"[Lclojure.lang.IFn$DD;" ifns ^long fns-len ^double input]
  (loop [i 0 v 0.0]
    (if (< i fns-len)
      (recur (unchecked-inc i) 
             (+ v (.invokePrim ^IFn$DD (aget ifns i) input)))
      v)))

(defn- ser
  ^double [^"[Lclojure.lang.IFn$DD;" ifns ^long fns-len ^double input]
  (loop [i 0 v input]
    (if (< i fns-len)
      (recur (unchecked-inc i) 
             (.invokePrim ^IFn$DD (aget ifns i) v))
      v)))

(defn freeverbm "Freeverb (mono) for single channel audio function."
  [afn ^double room ^double damp ^long spread]
  ;(println "Freeverbm " room " " damp " " spread)
  (let [^doubles out (create-buffer)
        ^"[Lclojure.lang.IFn$DD;"
        combs (into-array 
                IFn$DD 
                (map (fn [^long tuning] 
                       (frvb-comb (adjust-tuning (+ spread tuning)) 
                                  room damp)) 
                     COMB-TUNING))
        ^"[Lclojure.lang.IFn$DD;"
        allpasses (into-array 
                    IFn$DD 
                    (map (fn [^long tuning] 
                           (frvb-allpass (adjust-tuning (+ spread tuning)) 
                                         ALLPASS-FEEDBACK)) 
                         ALLPASS-TUNING))
        combs-len (alength combs)
        allpasses-len (alength allpasses)]
    (generator
      [] [input afn]
      (let [comb-val (par combs combs-len input) 
            out-val (ser allpasses allpasses-len comb-val)]
        (aset out int-indx out-val)
        (gen-recur))
      (yield out))))

;;todo - add wet/dry balance
(defn freeverb
  "Freeverb (stereo) for two-channel audio function."
  [afn ^double room-size ^double hf-damping]
  (with-signals [[left right] afn]
    (let [out ^"[[D" (create-buffers 2) 
          combined (shared (mul FIXED-GAIN (sum left right)))
          scaledamp 0.4
          scaleroom 0.28
          offsetroom 0.7
          sr-mult (/ ORIG-SR (double *sr*))
          damp (* hf-damping scaledamp) 
          room (+ (* room-size scaleroom) offsetroom)
          freeverbL (freeverbm combined room damp 0)
          freeverbR (freeverbm combined room damp DEFAULT-STEREO-SPREAD)]
      (fn []
        (let [a (freeverbL)
              b (freeverbR)]
          (when (and a b)
            (aset out 0 a)
            (aset out 1 b))
            out)))))

;(let [c (frvb-allpass 556 0.5)]
;  (loop [i 0]
;    (println i ": " (c 0.5))
;    (when (< i (* 20 556))
;      (recur (unchecked-inc i)))))

;(require '[pink.oscillators :refer [pulse]])
;(require '[clojure.pprint :refer [pprint]])
;(let [f (freeverbm (pulse 0) 0.7 0.2 0)]
;  (doseq [x (range 100)] 
;    (pprint (f)))  
;  )
