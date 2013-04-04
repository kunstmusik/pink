(ns audio-seq.engine
  "Audio Engine Code"
  (:import (java.nio ByteBuffer)
           (javax.sound.sampled AudioFormat AudioSystem
                                SourceDataLine)))

(def af (AudioFormat. 44100 16 1 true true))

(def buffer-size 1024)

(def ^:dynamic *sr* 44100)
(def ^:dynamic *ksmps* 64)

;;;; Engine

(defn engine-create []
  "Creates an engine"
  {:status (atom :stopped)
   :line nil
   :current-audio-funcs nil})

(defn engine-start [engine]
  (when (= @(:status engine) :stopped)
    (swap! (:status engine) :initializing)
    (comment do setup, open audio line, start running)
    ))

(defn engine-stop [engine]
  (when (not= @(:status engine) :running)
    (println "Stopping Engine")))

(defn engine-status [engine]
  @(:status engine))

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

(defn run-audio-block [a-block]
  (let [#^SourceDataLine line (open-line af)
        audio-block a-block]
    (let [cnt (/ (* *sr* 5.0) buffer-size)
        buffer (ByteBuffer/allocate buffer-size)
        write-buffer-size (/ buffer-size 2)
        frames (quot write-buffer-size *ksmps*)]
      (loop [c cnt] 
       (when (pos? c) 
         (loop [x 0]
           (when (< x frames)
             (let [buf ^doubles (a-block)]
               (loop [y 0]
                 (when (< y (alength buf))
                   (.putShort buffer (short (* Short/MAX_VALUE (aget buf y))))
                   (recur (unchecked-inc y)))) 
               (recur (unchecked-inc x)))))
         (.write line (.array buffer) 0 buffer-size)
         (.clear buffer)
      (recur (dec c) ))))
    (.close line)))
