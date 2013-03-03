(ns audio-seq.core4
  (:import (javax.sound.sampled
                       AudioFormat
                       AudioFormat$Encoding
                       AudioFileFormat
                       AudioFileFormat$Type
                       AudioInputStream
                       AudioSystem
           DataLine$Info SourceDataLine)
        (java.nio ByteBuffer)
        (java.io File ByteArrayInputStream)))

(def af (AudioFormat. 44100 16 1 true true))

(def buffer-size 1024)

(def ^:dynamic *sr* 44100)
(def ^:const PI Math/PI)
(def ^:const KSMPS 64)

(defn getd ^double [^doubles a] (aget a 0))
(defn setd! ^double [^doubles a ^double v] (aset a 0 v))
(defn getl ^long [^longs a] (aget a 0))
(defn setl! ^long [^longs a ^long v] (aset a 0 v))

(definline swapd! [d f] 
  `(setd! ~d (~f (getd ~d))))

(definline swapl! [l f]
  `(setl! ~l (~f (getl ~l))))

(defn create-buffer 
  ([] (double-array KSMPS))
  ([i] (double-array KSMPS i)))

(def empty-d ^doubles (create-buffer 0))

(defn clear-d [^doubles d]
  (System/arraycopy empty-d 0 d 0 (alength ^doubles empty-d)))

(defn map-d 
  ([f ^doubles a ^doubles b]
    (let [l (alength a)]
      (loop [cnt 0]
        (when (< cnt l)
          (aset ^doubles b cnt ^double (f (aget ^doubles a cnt)))
          (recur (unchecked-inc cnt))))
      b))
  ([f ^doubles a ^doubles b ^doubles c]
    (let [l (alength a)]
      (loop [cnt 0]
        (when (< cnt l)
          (aset ^doubles c cnt ^double (f (aget ^doubles a cnt) (aget ^doubles b cnt)))
          (recur (unchecked-inc cnt))))
      c)))

(defn reduce-d
  ([f ^doubles out fns]
    (clear-d out)
    (loop [[x & xs] fns]
      (when x
        (let [buf ^doubles (x)
              len (alength buf)]
          (loop [cnt 0]
            (when (< cnt len) 
              (aset out cnt ^double (f (aget ^doubles out cnt) (aget ^doubles buf cnt)))
              (recur (unchecked-inc cnt)))))
        (recur xs)))
   out))
        
(defn fill 
  "Fills double[] buf with values. Initial value is set to value from double[] start, 
  then f called like iterate with the value.  Last value is stored back into the start.
  Returns buf at end."
  [^doubles buf ^doubles start f]
  (let [len (alength buf)
        lastindx (- len 1)]
    (loop [cnt (unchecked-long 0)]
      (if (< cnt len)
;        (let [tmp ^double (f v)] 
;          (aset ^doubles buf cnt ^double tmp)
;          (recur (unchecked-inc cnt) ^double tmp))
          (do
            (aset ^doubles buf cnt (swapd! start #(f ^double %)))
            (recur (unchecked-inc cnt)))
        buf))))

(defn dec-if ^double [^double a] (if (> a 1) (dec a) a))

(defn phasor2 [^double freq ^double phase]
  (let [phase-incr ^double (/ freq  *sr*)
        cur-phase ^doubles (double-array 1 phase)
        out ^doubles (create-buffer)]
      (fn ^doubles [] 
        (fill out cur-phase #(dec-if (+ phase-incr ^double %))))))

(defn sinev [^double freq ^double phase]
  (let [phasor (phasor2 freq phase)
        out ^doubles (create-buffer)]
    (fn ^doubles []
      (map-d #(Math/sin (* 2.0 PI ^double %)) (phasor) out))))

(defn ^doubles mul-d [^doubles a ^doubles b ^doubles out]
   (map-d #(* ^double %1 ^double %2) a b out))

(defn mul [a b]
  (let [out ^doubles (create-buffer)]
    (fn ^doubles []
      (map-d #(* ^double %1 ^double %2) ^doubles (a) ^doubles (b) out))))

(defn const [^double a]
  (let [out ^doubles (create-buffer a)]
  (fn ^doubles []
    out)))

(defn mix
  [& args]
    (if (> (count args) 1)
      (let [tmp (create-buffer)
            out (create-buffer)
          adjust (create-buffer (/ 1.0 (count args)))]
        (fn ^doubles []
          (mul-d adjust (reduce-d #(+ ^double %1 ^double %2) tmp args) out)))
      (nth args 0)))

(defn make-env-data [pts]
  {:pre (even? (count pts))}
  (let [[x & xs] (partition 2 pts)]
    (second (reduce (fn [[[a b] lst] [c d :as p]] 
              (let [run (double (* c *sr*))
                   rise (double (/ (- d b) run))] 
             [p (conj lst [run rise])] ))
                         [x []] xs))))

(defn env-get-inc [data counter]
  (loop [cnt 0.0 [x & xs] data]
    (if x
      (let [[a b] x
            c (+ cnt a)]
        (if (< counter c)
          b
          (recur c xs))) 
      0.0)))


(defn env [pts]
 {:pre (even? (count pts))}
  (let [linedata (make-env-data pts)
        cur-val (double-array 1 (nth pts 0))
        counter (long-array 1 -1)
        out (create-buffer)]
  (fn ^doubles[]
    (fill out cur-val #(+ ^double % ^double (env-get-inc linedata (swapl! counter inc)))))))
    

; JAVASOUND CODE

(defn open-line [audio-format]
  (let [#^SourceDataLine line (AudioSystem/getSourceDataLine audio-format)]
    (doto line 
    (.open audio-format)
    (.start))))

(defn run-audio-block2 [a-block]
  (let [#^SourceDataLine line (open-line af)
        audio-block a-block]
    (let [cnt (/ (* *sr* 5.0) buffer-size)
        buffer (ByteBuffer/allocate buffer-size)
        write-buffer-size (/ buffer-size 2)
        frames (quot write-buffer-size KSMPS)]
      (loop [c cnt] 
       (when (> c 0) 
         (loop [x 0]
           (when (< x frames)
             (let [buf ^doubles (a-block)]
               (loop [y 0]
                 (when (< y (alength buf))
                   (.putShort buffer (short (* Short/MAX_VALUE ^double (aget ^doubles buf y))))
                   (recur (unchecked-inc y)))) 
               (recur (unchecked-inc x)))))
         (.write line (.array buffer) 0 buffer-size)
         (.clear buffer)
      (recur (dec c) ))))
    (.close line)))

(defn audio-block3 [x]
  (mul
    (apply mix 
     (map #(sinev (* % 60) 0)
        (take x (iterate inc 1))))
    (env [0.0 0.0 0.05 1 0.05 0.9 0.5 0.9 0.5 0])))

(defn demo3 [x] (run-audio-block2 (audio-block3 x)))

