(ns audio-seq.core)

(import '(javax.sound.sampled
                       AudioFormat
                       AudioFormat$Encoding
                       AudioFileFormat
                       AudioFileFormat$Type
                       AudioInputStream
                       AudioSystem
           DataLine$Info SourceDataLine)
        '(java.nio ByteBuffer)
        '(java.io File ByteArrayInputStream))

(def af (AudioFormat. 44100 16 1 true true))

(def buffer-size 1024)

(def ^:dynamic *sr* 44100)


;(def audio-stream (AudioInputStream/getAudioInputStream
;(def f (File/createTempFile "tmp" ".raw" (File. "."))) 
;(println (bean f))

(defn phasor [freq phase]
  (let [phase-incr (/ freq  *sr*)]
    (iterate #(let [p (+ % phase-incr)] (if (> p 1) (dec p) p)) phase)))

(defn sine-osc [freq phase]
    (map #(Math/sin (* 2 Math/PI %)) (phasor freq phase)))

(defn amix 
  ([] [])
  ([& a] 
     (let [len (count a)]
       (if (= len 1)
         (first a) 
         (map #(reduce + %) (partition len (apply interleave a)))))))


(def audio-block
  (map #(* 0.25 %)
  (amix 
    (sine-osc 440.0 0)
    (sine-osc 660.0 0)
    (sine-osc 990.0 0)
    (sine-osc 1220.0 0)
    ))
  )


;(def audio-block
;    (sine-osc 660.0 0)
;    )

; JAVASOUND CODE

(defn open-line [audio-format]
  (let [#^SourceDataLine line (AudioSystem/getSourceDataLine audio-format)]
    (doto line 
    (.open audio-format)
    (.start))))

(def #^SourceDataLine line (open-line af))

(let [cnt (/ (* *sr* 5.0) buffer-size)
      buffer (ByteBuffer/allocate buffer-size)]
  (loop [c cnt 
         [x & xs] (partition (/ buffer-size 2) audio-block)] 
    (when (> c 0)
      (loop [[a & b] x]
        (when a
          (.putShort buffer (.shortValue (* Short/MAX_VALUE a)))
          (recur b))) 
      (.write line (.array buffer) 0 buffer-size)
      (.clear buffer)
      (recur (dec c) xs ))))

(.close line)
        


;(def audio-out-proxy 
;  (proxy [AudioInputStream]
;    (

