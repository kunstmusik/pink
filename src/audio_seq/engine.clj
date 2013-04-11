(ns audio-seq.engine
  "Audio Engine Code"
  (:import 
    (java.util Arrays)
    (java.nio ByteBuffer)
    (javax.sound.sampled AudioFormat AudioSystem
                                SourceDataLine)))


(defn map-d 
  "Maps function f across double[] buffers and writes output to final passed in buffer" 
  ([f ^doubles a ^doubles b]
    (when (and a b)
      (let [l (alength a)]
        (loop [cnt 0]
          (when (< cnt l)
            (aset b cnt ^double (f (aget a cnt)))
            (recur (unchecked-inc cnt))))
        b)))
  ([f ^doubles a ^doubles b ^doubles c]
    (when (and a b c)
      (let [l (alength a)]
        (loop [cnt 0]
          (when (< cnt l)
            (aset c cnt ^double (f (aget a cnt) (aget b cnt)))
            (recur (unchecked-inc cnt))))
        c))))

(def af (AudioFormat. 44100 16 1 true true))

(def ^:dynamic *sr* 44100)
(def ^:dynamic *ksmps* 64)

(def buffer-size 256)
(def write-buffer-size (/ buffer-size 2))
(def frames (quot write-buffer-size *ksmps*))



(defn ^short limit [^double num]
  (if (> num Short/MAX_VALUE)
    Short/MAX_VALUE 
    (if (< num Short/MIN_VALUE)
      Short/MIN_VALUE 
      (short num))))

;;;; Engine

(defn engine-create []
  "Creates an engine"
  (agent {:status :stopped
   :line nil
   :out-buffer (double-array *ksmps*)
   :buffer (ByteBuffer/allocate buffer-size)
   :audio-funcs []}))


;; should do initialization of f on separate thread?
(defn engine-add-active-func [engine f]
  (println "adding audio function"))

(defn engine-remove-func [engine f]
  (println "removing audio function")) 

;;;; JAVASOUND CODE

(defn open-line [audio-format]
  (let [#^SourceDataLine line (AudioSystem/getSourceDataLine audio-format)]
    (doto line 
    (.open audio-format)
    (.start))))

(defn map-over-d [f ^doubles buf]
  (let [len (alength buf)]
    (loop [y 0]
      (when (< y len)
        (f (aget buf y))
        (recur (unchecked-inc y))))))

(defn run-audio-funcs [afs ^doubles buffer]
  (loop [[x & xs] afs ret []]
    (if x 
      (let [b (x)]
        (if b
          (do 
            (map-d + buffer b buffer)
            (recur xs (conj ret x)))
          (recur xs ret)))
     ret))) 

(defn process-frame 
  [afuncs ^doubles outbuffer ^SourceDataLine line ^ByteBuffer buffer frames]
  (loop [x 0 afs afuncs]
    (if (< x frames)
      (do
        (Arrays/fill ^doubles outbuffer 0.0)
        (let [newfs (run-audio-funcs afs outbuffer)]
          (map-over-d #(.putShort buffer (limit (* Short/MAX_VALUE %))) outbuffer)
          (recur (inc x) newfs)))
      (do
        (.write line (.array buffer) 0 buffer-size)
        (.clear buffer)
        afs))))

(defn engine-run [engine a]
  (if (= (engine :status) :running)
    (let [line (engine :line)
          outbuf (engine :out-buffer)
          buf (engine :buffer)
          afs (process-frame (engine :audio-funcs) outbuf line buf frames)
          neweng (assoc engine :audio-funcs afs)]
      (send a engine-run a)
      neweng)
    (do
      (println "stopping...")
      (doto ^SourceDataLine (engine :line)
        (.flush)
        (.close))
      engine)))

(defn engine-start [engine]
  (when (= (:status @engine) :stopped)
    (let [#^SourceDataLine line (open-line af)]
      (send engine assoc :status :running :line line)
      (send engine engine-run engine))))

(defn engine-stop [engine]
  (when (= (:status @engine) :running)
    (send engine assoc :status :stopped :line nil)))

(defn engine-status [engine]
  (:status @engine))


(defn run-audio-block [a-block]
  (let [#^SourceDataLine line (open-line af) 
        buffer (ByteBuffer/allocate buffer-size)
        write-buffer-size (/ buffer-size 2)
        frames (quot write-buffer-size *ksmps*)]
    (loop [x 0]
      (if (< x frames)
        (if-let [buf ^doubles (a-block)]
          (do
            (map-over-d #(.putShort buffer (limit (* Short/MAX_VALUE %))) buf)
            (recur (unchecked-inc x)))
          (do
            (.write line (.array buffer) 0 buffer-size)
            (.clear buffer)))
        (do
          (.write line (.array buffer) 0 buffer-size)
          (.clear buffer)
          (recur 0))))
    (.flush line)
    (.close line)))
