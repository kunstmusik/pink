(ns pink.engine
  "Audio Engine Code"
  (:require [pink.config :refer :all]
            [pink.util :refer :all]
            [pink.event :refer :all]
            [pink.io.audio :refer :all]
            [pink.node :refer :all]
            [pink.io.sound-file :refer :all])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream File FileOutputStream DataOutputStream] 
           [java.nio ByteBuffer]
           [java.util Arrays]
           [javax.sound.sampled AudioFormat AudioSystem SourceDataLine
                                AudioFileFormat$Type AudioInputStream]
           [pink EngineUtils Operator]
           [pink.event Event]))


(def ^:const windows? 
  (-> (System/getProperty "os.name")
      (.toLowerCase)
      (.startsWith "windows")))

(def ^:dynamic *hardware-buffer-size*
  (if windows? 1024 256))

;;;; Engine

(def engines (atom []))

(def ^:constant BYTE-SIZE (/ Short/SIZE Byte/SIZE)) ; 2 bytes for 16-bit audio

(deftype Engine [status clear 
                   pre-cfunc-node
                   root-audio-node
                   post-cfunc-node
                   ^long sample-rate ^long nchnls ^long buffer-size
                   ^long out-buffer-size ^long byte-buffer-size
                   event-list ]
  Object
  (toString [this] (str "[Engine] ID: " (System/identityHashCode this) " Status " @status)))

(defn engine-create 
  "Creates an audio engine"
  [& {:keys [sample-rate nchnls buffer-size] 
      :or {sample-rate 44100 nchnls 1 buffer-size 64}}] 
  (let  [bsize (long buffer-size)
         channels (long nchnls)
         e 
          (Engine. (atom :stopped) (atom false) 
            (create-node)
            (create-node :channels channels)
            (create-node)
            sample-rate channels bsize 
            (* bsize channels) (* (long BYTE-SIZE) bsize channels)
            (event-list bsize sample-rate))]
    (swap! engines conj e) 
    e))

;; should do initialization of f on separate thread?
(defn engine-add-afunc 
  [^Engine engine f]
  (node-add-func (.root-audio-node engine) f))

(defn engine-remove-afunc 
  [^Engine engine f]
  (node-remove-func (.root-audio-node engine) f)) 

(defn engine-add-pre-cfunc 
  [^Engine engine f]
  (node-add-func (.pre-cfunc-node engine) f))

(defn engine-remove-pre-cfunc 
  [^Engine engine f]
  (node-remove-func (.pre-cfunc-node engine) f)) 

(defn engine-add-post-cfunc 
  [^Engine engine f]
  (node-add-func (.post-cfunc-node engine) f))

(defn engine-remove-post-cfunc 
  [^Engine engine f]
  (node-remove-func (.post-cfunc-node engine) f))

(defn engine-add-events 
  [^Engine engine events]
  (event-list-add (.event-list engine) events))

(defn engine-get-tempo-atom
  [^Engine engine]
  (event-list-get-tempo-atom (.event-list engine)))

(defn engine-get-tempo
  [^Engine engine]
  @(engine-get-tempo-atom engine))

(defn engine-set-tempo
  [^Engine engine tempo]
  (reset! (engine-get-tempo-atom engine) tempo))

;;;; JAVASOUND CODE

(defn doubles->byte-buffer 
  "Write output from doubles array into ByteBuffer. 
  Maps -1.0,1.0 to Short/MIN_VALUE,Short/MAX_VALUE, truncating
  values outside of -1.0,1.0."
  [^doubles dbls ^ByteBuffer buf]
  (EngineUtils/writeDoublesToByteBufferAsShorts dbls buf))


(defn- write-interleaved 
  [^doubles out-buffer asig ^long num-channels ^long buffer-size]
  (if (= num-channels 1)
    (System/arraycopy ^doubles asig 0 out-buffer 0 buffer-size)
    (loop [i 0]
      (when (< i num-channels) 
        (let [^doubles channel (aget ^"[[D" asig i)]
          (loop [j 0]
            (when (< j buffer-size) 
              (aset out-buffer (+ i (* j num-channels))
                    (aget channel j))
              (recur (unchecked-inc j)))))
        (recur (unchecked-inc i)))))
  out-buffer)

(defn- buf->line [^ByteBuffer buffer ^SourceDataLine line
                 ^long out-buffer-size]
  (.write line (.array buffer) 0 out-buffer-size)
  (.clear buffer))

(defn update-funcs
  [v adds-atm removes-atm]
  (let [removes (drain-atom! removes-atm)]
    (filter #(< (.indexOf ^"clojure.lang.PersistentVector" removes %) 0)
           (concat-drain! v adds-atm))))

(defn engine-run 
  "Main realtime engine running function. Called within a thread from
  engine-start."
  [^Engine engine]
  (let [nchnls (.nchnls engine) 
        sr (.sample-rate engine)
        buffer-size (.buffer-size engine)
        af (AudioFormat. (.sample-rate engine) 16 nchnls true true)
        #^SourceDataLine line (open-line af (* 16 nchnls (long *hardware-buffer-size*)))        
        out-buffer (double-array (.out-buffer-size engine))
        buf (ByteBuffer/allocate (.byte-buffer-size engine))
        pre-control (.pre-cfunc-node engine)
        root-audio-node (.root-audio-node engine)
        post-control (.post-cfunc-node engine)
        clear-flag (.clear engine)
        run-engine-events (event-list-processor (.event-list engine))
        run-pre-control-funcs (control-node-processor pre-control)
        run-audio-funcs (binding [*buffer-size* buffer-size] 
                          (node-processor root-audio-node))
        run-post-control-funcs (control-node-processor post-control)]

    (binding [*sr* sr *buffer-size* buffer-size *nchnls* nchnls]

      (loop [buffer-count 0]
        (if (= @(.status engine) :running)
          (do 
            (binding [*current-buffer-num* buffer-count]
              (run-engine-events)
              (run-pre-control-funcs)
              (-> out-buffer  
                  (write-interleaved (run-audio-funcs) nchnls buffer-size)
                  (doubles->byte-buffer buf))
              (run-post-control-funcs)) 
            (buf->line buf line (.byte-buffer-size engine))
            (when @clear-flag 
              (node-clear pre-control)
              (node-clear root-audio-node)
              (node-clear post-control)
              (reset! clear-flag false)
              (event-list-clear (.event-list engine)))
            (recur (unchecked-inc buffer-count)))
          (do
            (println "stopping...")
            (doto line
              (.flush)
              (.close))))))))

(defn engine-start [^Engine engine]
  (when (= @(.status engine) :stopped)
    (reset! (.status engine) :running) 
    (.start (Thread. ^Runnable (partial engine-run engine)))))

(defn engine-stop [^Engine engine]
  (when (= @(.status engine) :running)
    (reset! (.status engine) :stopped)))

(defn engine-clear 
  [^Engine engine]
  (if (= @(.status engine) :running)
    (reset! (.clear engine) true)))
                            
(defn engine-status 
  [^Engine engine]
  @(.status engine))

(defn engine-kill-all
  "Kills all engines and clears them"
  []
  (loop [[a & b] (drain-atom! engines)]
    (when a
      (engine-clear a)
      (engine-stop a)
      (recur b)
      )))

(defn engines-clear
  "Kills all engines and clears global engines list. Useful for development in REPL, but user must be 
  careful after clearing not to use existing engines."
  []
  (engine-kill-all)
  (reset! engines []))

;; Non-Realtime Engine functions

;(defn write-aiff-header 
;  [fos nchnls] 
;  (doto (DataOutputStream. fos) 
;    ;; FORM CHUNK
;    (.writeBytes "FORM")
;    (.writeLong 0) ;; fill in later
;    (.writeBytes "AIFF")
;    ;; COMM CHUNK
;    (.writeBytes "COMM")
;    (.writeLong 18)
;    (.writeShort nchnls)
;    (.writeInt 0) ;; fill in later
;    (.writeShort 16) ;; try this as doubles later
;    (.write)
;    )
;  )

(defn engine->disk 
  "Runs engine and writes output to disk.  This will run the engine until all
  audio functions added to it are complete.  

  Warning: Be careful not to render with an engine setup with infinite
  duration!"
  [^Engine engine ^String filename]

  (let [sr (.sample-rate engine)
        buffer-size (.buffer-size engine)
        nchnls (.nchnls engine)
        baos (ByteArrayOutputStream.)
        out-buffer (double-array (.out-buffer-size engine))
        start-time (System/currentTimeMillis)
        pre-control (.pre-cfunc-node engine)
        root-audio-node (.root-audio-node engine)
        post-control (.post-cfunc-node engine)
        clear-flag (.clear engine)
        run-engine-events (event-list-processor (.event-list engine))
        run-pre-control-funcs (control-node-processor pre-control)
        run-audio-funcs (binding [*buffer-size* buffer-size] 
                          (node-processor root-audio-node))
        run-post-control-funcs (control-node-processor post-control) 
        wav-out (open-wave-write 
                  filename sr 16 nchnls buffer-size)]

    (reset! (.status engine) :running)

    (binding [*sr* sr *buffer-size* buffer-size *nchnls* nchnls]

      (loop [buffer-count 0]
        (if (= @(.status engine) :running)
          (do 
            (binding [*current-buffer-num* buffer-count]
              (run-engine-events)
              (run-pre-control-funcs)
              (-> out-buffer  
                  (write-interleaved (run-audio-funcs) nchnls buffer-size)
                  (write-wav-data wav-out))
              (run-post-control-funcs)) 
            (when @clear-flag 
              (node-clear pre-control)
              (node-clear root-audio-node)
              (node-clear post-control)
              (reset! clear-flag false)
              (event-list-clear (.event-list engine)))
            ;(println 
            ;  (node-empty? pre-control) " : "
            ;           (node-empty? root-audio-node) " : "
            ;           (node-empty? post-control) " : " 
            ;            (event-list-empty? (.event-list engine))

            ;  )
            (when (and (node-empty? pre-control) 
                       (node-empty? root-audio-node) 
                       (node-empty? post-control) 
                       (event-list-empty? (.event-list engine)))
              (engine-stop engine))
            ;(println "EVENTS: " (.events (.event-list engine)))
            (recur (unchecked-inc buffer-count)))
          (do
            (close-wav-data wav-out)
            (println "Output written to " (.getAbsolutePath (File. filename)))

            (println "Elapsed time: " 
                     (/ (- (System/currentTimeMillis) start-time) 1000.0)))
          )))))

;; Event functions dealing with audio engines

(defn fire-audio-event 
  "create an instance of an audio function and adds to the engine" 
  [eng evt]  
  (when-let [afunc (fire-event evt)] 
    (engine-add-afunc eng afunc)))

(defn wrap-audio-event 
  [eng ^Event evt]
  (wrap-event fire-audio-event [eng] evt))

(defn audio-events 
  "Takes an engine and series of events, wrapping the events as audio-events.
  If single arg given, assumes it is a list of events."
  ([eng args]
   (if (sequential? args)
     (map #(wrap-audio-event eng %) args)    
     (map #(wrap-audio-event eng %) [args])))
  ([eng x & args]
   (audio-events eng (list* x args))))

;; Utility Functions

(defn run-audio-block 
  "TODO: Fix this function and document..."
  [a-block & {:keys [sample-rate nchnls block-size] 
              :or {sample-rate 44100 nchnls 1 block-size 256}}]
  ;(let [af (AudioFormat. sample-rate 16 nchnls true true)
  ;      #^SourceDataLine line (open-line af) 
  ;      buffer (ByteBuffer/allocate buffer-size)
  ;      write-buffer-size (/ buffer-size 2)
  ;      frames (quot write-buffer-size *buffer-size*)]
  ;  (loop [x 0]
  ;    (if (< x frames)
  ;      (if-let [buf ^doubles (a-block)]
  ;        (do
  ;          (doubles->byte-buffer buf buffer)
  ;          (recur (unchecked-inc x)))
  ;        (do
  ;          (.write line (.array buffer) 0 buffer-size)
  ;          (.clear buffer)))
  ;      (do
  ;        (.write line (.array buffer) 0 buffer-size)
  ;        (.clear buffer)
  ;        (recur 0))))
  ;  (.flush line)
  ;  (.close line))
  
  )

