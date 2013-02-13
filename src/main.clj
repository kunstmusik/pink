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


(defn open-line [audio-format]
  (let [#^SourceDataLine line (AudioSystem/getSourceDataLine audio-format)]
    (doto line 
    (.open audio-format)
    (.start))))

(def #^SourceDataLine line (open-line af))

(let [cnt (/ (* *sr* 5.0) buffer-size)
      buffer (ByteBuffer/allocate buffer-size)]
  (loop [c cnt 
         [x & xs] (partition (/ buffer-size 2) (sine-osc 440.0 0))] 
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

