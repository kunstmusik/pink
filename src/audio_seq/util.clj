(ns audio-seq.util
  "Audio utility code for working with buffers (double[])"
  (:require [audio-seq.engine :refer [*ksmps*]]))

(defn getd ^double [^doubles a] (aget a 0))
(defn setd! ^double [^doubles a ^double v] (aset a 0 v))
(defn getl ^long [^longs a] (aget a 0))
(defn setl! ^long [^longs a ^long v] (aset a 0 v))


;(defn ^double swapd! [d f] 
;  (setd! d (f (getd d))))

(definline swapd! [d f] 
  `(setd! ~d (~f (getd ~d))))

;(defn ^long swapl! [l f]
;  (setl! l (f (getl l))))

(definline swapl! [l f]
  `(setl! ~l (~f (getl ~l))))

(defn create-buffer 
  ([] (double-array *ksmps*))
  ([i] (double-array *ksmps* i)))

(def empty-d (create-buffer 0))

(defn clear-d [^doubles d]
  (System/arraycopy empty-d 0 d 0 (alength ^doubles empty-d)))

(defn map-d 
  "Maps function f across double[] buffers and writes output to final passed in buffer" 
  ([f ^doubles a ^doubles b]
    (let [l (alength a)]
      (loop [cnt 0]
        (when (< cnt l)
          (aset b cnt ^double (f (aget a cnt)))
          (recur (unchecked-inc cnt))))
      b))
  ([f ^doubles a ^doubles b ^doubles c]
    (let [l (alength a)]
      (loop [cnt 0]
        (when (< cnt l)
          (aset c cnt ^double (f (aget a cnt) (aget b cnt)))
          (recur (unchecked-inc cnt))))
      c)))

(defn reduce-d
  "calls f on buffers generates from fns in a manner similar to reduce, 
  writing the reduced values into out buffer"
  ([f ^doubles out fns]
    (clear-d out)
    (loop [[x & xs] fns]
      (when x
        (let [buf ^doubles (x)
              len (alength buf)]
          (loop [cnt 0]
            (when (< cnt len) 
              (aset out cnt ^double (f (aget out cnt) (aget buf cnt)))
              (recur (unchecked-inc cnt)))))
        (recur xs)))
   out))
        
(defn fill 
  "Fills double[] buf with values. Initial value is set to value from double[] start, 
  then f called like iterate with the value.  Last value is stored back into the start.
  Returns buf at end."
  [^doubles buf ^doubles start f]
  (let [len (alength buf)
        lastindx (dec len)]
    (loop [cnt (unchecked-long 0)]
      (when (< cnt len)
        (aset ^doubles buf cnt ^double (swapd! start f))
        (recur (unchecked-inc cnt))))
    buf))


(defn ^doubles mul-d [^doubles a ^doubles b ^doubles out]
   (map-d * a b out))

(defn mul [a b]
  (let [out (create-buffer)]
    (fn ^doubles []
      (map-d * ^doubles (a) ^doubles (b) out))))

(defn const [^double a]
  (let [out (create-buffer a)]
  (fn ^doubles []
    out)))

(defn mix
  [& args]
    (if (> (count args) 1)
      (let [tmp (create-buffer)
            out (create-buffer)
          adjust (create-buffer (/ 1.0 (count args)))]
        (fn ^doubles []
          (mul-d adjust (reduce-d + tmp args) out)))
      (nth args 0)))
