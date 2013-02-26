(ns audio-seq.core2
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


(defn ^double dec-if [^double a] (if (> a 1) (dec a) a))

(defn val-copy [^doubles a ^doubles b]
  (do (aset b 0 (aget a 0))))

(defn ^double val-get [^doubles a] (aget a 0))

(defn ^doubles phasor2 [^double freq ^double phase]
    (let [phase-incr ^double (/ freq  *sr*)
          phase-val ^doubles (double-array 1)]
      (aset phase-val 0 phase)
      (fn [] 
        (let [v ^double (dec-if (+ phase-incr (aget ^doubles phase-val 0)))]
          (aset ^doubles phase-val 0 ^double v)
          phase-val))))

(defn ^doubles sinev [^double freq ^double phase]
  (let [vals (double-array 1)
        phasor (phasor2 freq phase)]
    (fn []
      (let [p (aget ^doubles (phasor) 0)
            v ^double (Math/sin (* 2.0 PI p))]
        (aset ^doubles vals 0 v)
        vals))))

(defn ^doubles amulv [^doubles a ^double v] 
  (do
    (aset a 0 (* v (aget a 0)))
    a))

(defn ^doubles mixf [a & args]
  (let [vals (a)]
    (fn []
      (let [v ^double (reduce #(+ ^double %1 (aget ^doubles (%2) 0)) (val-get vals) args)]
        (aset ^doubles vals 0 ^double v)
        (amulv vals (/ 1.0 (count args)))
        vals))))

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
        write-buffer-size (/ buffer-size 2)]
      (loop [c cnt] 
       (when (> c 0) 
         (loop [x 0]
           (when (< x write-buffer-size)
             (.putShort buffer (short (* Short/MAX_VALUE (aget ^doubles (a-block) 0))))
             (recur (inc x)))) 
         (.write line (.array buffer) 0 buffer-size)
         (.clear buffer)
      (recur (dec c) ))))
    (.close line)))

(defn audio-block3 []
  (apply mixf 
   (map #(sinev (* % 60) 0)
      (take 90 (iterate inc 1)))))

(defn demo3 [] (run-audio-block2 (audio-block3)))

