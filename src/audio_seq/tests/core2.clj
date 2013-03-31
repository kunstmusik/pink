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


(defn dec-if ^double [^double a] (if (> a 1) (dec a) a))

(defn val-copy [^doubles a ^doubles b]
  (do (aset b 0 (aget a 0))))

(defn getv [^doubles a] (aget a 0))
(defn setv! [^doubles a ^double v] (do (aset a 0 v) a))

(defn phasor2 [^double freq ^double phase]
    (let [phase-incr ^double (/ freq  *sr*)
          phase-val ^doubles (double-array 1 phase)
          out ^doubles (double-array 1 phase)]
      (fn [] 
        (let [v (dec-if (+ phase-incr (^double getv phase-val)))]
          (setv! phase-val v)
          (setv! out v)))))

(defn sinev [^double freq ^double phase]
  (let [out (double-array 1)
        phasor (phasor2 freq phase)]
    (fn []
      (let [p ^double (getv (phasor))
            v (Math/sin (* 2.0 PI p))]
        (setv! out v)))))

(defn mul [a b]
  (let [out (double-array 1)]
  (fn []
    (let [bufa ^doubles (a) bufb ^doubles (b)]
    (setv! out (* (^double getv bufa) (^double getv bufb)))))))

(defn mix
  ([a] a)
  ([a & args]
  (let [out (double-array 1)
        adjust (/ 1.0 (+ 1 (count args)))]
    (fn []
      (let [v ^double (* adjust (reduce #(+ ^double %1 (^double getv (%2))) (getv (a)) args))]
        (setv! out v))))))

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
        out (double-array 1 (nth pts 0))
        counter (atom -1)]
  (fn []
    (let [v (+ (^double getv cur-val) (env-get-inc linedata (swap! counter inc)))]
      (setv! cur-val v) 
      (setv! out v)))))
    

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

(defn audio-block3 [x]
  (mul
    (apply mix 
     (map #(sinev (* % 60) 0)
        (take x (iterate inc 1))))
    (env [0.0 0.0 0.05 1 0.05 0.9 0.5 0.9 0.5 0])
    ))

(defn demo3 [x] (run-audio-block2 (audio-block3 x)))

