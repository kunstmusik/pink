(ns pink.audio.engine
  "Audio Engine Code"
  (:import (java.nio ByteBuffer)
           (java.util Arrays)
           (javax.sound.sampled AudioFormat AudioSystem SourceDataLine)))

(defn- tagit 
  [a t]
  (with-meta a {:tag t}))

(defn- tag-double
  [a]
  (tagit a "double"))

(defmacro map-d-impl
  [out f & buffers]  
  (let [cnt (gensym 'count)
        get-bufs (map (fn [a] (list 'aget a cnt)) buffers )
        apply-line `(~f ~@get-bufs)
        ] 
    `(when (and ~@buffers)
     (let [l# (alength ~out)]
       (loop [~cnt (unchecked-int 0)]
         (when (< ~cnt l#)
           (aset ~out ~cnt
                  ~(tag-double apply-line)) 
           (recur (unchecked-inc-int ~cnt))
           ))
       ~out
       )    
     )))

(defn- map-d 
  "Maps function f across double[] buffers and writes output to out buffer" 
  ([^doubles out f ^doubles x]
    (map-d-impl out f x)   
   )
  ([^doubles out f ^doubles x ^doubles y ]
    (map-d-impl out f x y)   
   )
  ([^doubles out f ^doubles x ^doubles y  ^doubles z]
    (map-d-impl out f x y z)   
   )
  )
 



(def af (AudioFormat. 44100 16 1 true true))

(def ^:dynamic *sr* 44100)
(def ^:dynamic *ksmps* 64)
(def ^:dynamic *nchnls* 1)
(def ^:dynamic *current-buffer-num* 0)

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

(def engines (ref []))

(defn engine-create 
  "Creates an audio engine"
  [] 
  (let  [e {:status (ref :stopped)
            :clear (ref false)
            :audio-funcs (ref [])
            :pending-funcs (ref [])
            }]
    (dosync (alter engines conj e))
    e))


;; should do initialization of f on separate thread?
(defn engine-add-afunc [engine f]
  (dosync (alter (engine :pending-funcs) conj f)))

(defn engine-remove-afunc [engine f]
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
            (map-d buffer + b buffer)
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

(defn process-buffer
  [afs ^doubles outbuffer ^ByteBuffer buffer]
  (Arrays/fill ^doubles outbuffer 0.0)
  (let [newfs (run-audio-funcs afs outbuffer)]
    (map-over-d #(.putShort buffer (limit (* Short/MAX_VALUE %))) outbuffer)
    newfs))

(defn buf->line [^ByteBuffer buffer ^SourceDataLine line]
  (.write line (.array buffer) 0 buffer-size)
  (.clear buffer))

(defn engine-run2 [engine]
  (let [#^SourceDataLine line (open-line af)        
        outbuf (double-array *ksmps*)
        buf (ByteBuffer/allocate buffer-size)
        audio-funcs (engine :audio-funcs)
        pending-funcs (engine :pending-funcs)
        clear-flag (engine :clear)
        bufnum (atom -1)
        ]
    (loop [frame-count 0]
      (if (= @(engine :status) :running)
        (let [f-count (rem (inc frame-count) frames)
              afs  (binding [*current-buffer-num* 
                             (swap! bufnum unchecked-inc-int)]
                (process-buffer @audio-funcs outbuf buf))]  
          (dosync
            (if @clear-flag
              (do
                (ref-set audio-funcs [])
                (ref-set pending-funcs [])
                (ref-set clear-flag false))
              (if (empty? @pending-funcs)
                (ref-set audio-funcs afs)
                (do
                  (ref-set audio-funcs (concat afs @pending-funcs))
                  (ref-set pending-funcs [])))))
          (when (zero? f-count)
            (buf->line buf line))
          (recur (long f-count)))
        (do
          (println "stopping...")
          (doto line
            (.flush)
            (.close)))))))

(defn engine-start [engine]
  (when (= @(engine :status) :stopped)
    (dosync (ref-set (engine :status) :running))
    (.start (Thread. ^Runnable (partial engine-run2 engine)))))

(defn engine-stop [engine]
  (when (= @(engine :status) :running)
    (dosync (ref-set (engine :status) :stopped))))

(defn engine-clear [engine]
  (if (= @(engine :status) :running)
    (dosync 
      (ref-set (engine :clear) true))
    (dosync 
      (ref-set (engine :audio-funcs) [])
      (ref-set (engine :pending-funcs) []))))
    
                            
(defn engine-status [engine]
  @(:status engine))

(defn engine-kill-all
  "Kills all engines and clears them"
  []
  (dosync
    (loop [[a & b] @engines]
      (when a
        (engine-clear a)
        (recur b)
        ))))

(defn engines-clear
  "Kills all engines and clears global engines list. Useful for development in REPL, but user must be 
  careful after clearing not to use existing engines."
  []
  (engine-kill-all)
  (dosync (ref-set engines [])))

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
