(ns pink.node
  "Nodes are audio-rate functions that aggregate audio from other audio-rate functions. Nodes can contain other nodes. Each Node is wrapped in pink.util.shared so that the output of Node may be used by multiple other audio-functions within the same time period.
  
  In general, users will first call create-node to create a node map. node-processor will be used as the audio-rate function to add to an Engine, Node, or other audio-function.  
  "
  (:require [pink.config :refer [*nchnls* *buffer-size*]]
            [pink.util :refer [create-buffer fill map-d 
                                     swapd! setd! getd arg]]
            )
  )

; TODO - need to change code that writes interleaved audio and instead writes discrete channels

; move to pink.util ...
(defn write-asig
  [^doubles out-buffer ^doubles asig chan-num]
  (if (= *nchnls* 1)
    (when (= 0 chan-num)
      (map-d out-buffer + out-buffer asig))
    (loop [i 0]
      (when (< i *buffer-size*)
        (let [out-index (+ chan-num (* i *nchnls*))] 
          (aset out-buffer out-index
            (+ (aget out-buffer out-index) (aget asig i))))
        (recur (unchecked-inc-int i))))))


(def DOUBLE-ARRAY-CLASS
  (type (double-array 1)))

(defmacro multi-channel?
  [buffer]
  `(not= DOUBLE-ARRAY-CLASS (type ~buffer)))

(defmacro run-audio-funcs [afs buffer]
  (let [x (gensym)
        b (gensym)]
   `(loop [[~x & xs#] ~afs 
          ret# []]
    (if ~x 
      (let [~b (~x)]
        (if ~b
          (do 
            (if (multi-channel? ~b)
              (loop [i# 0 len# (count ~b)]
                (when (< i# len#)
                  (write-asig ~buffer 
                              (aget ~(with-meta b {:tag "[[D"}) i#) i#)
                  (recur (unchecked-inc-int i#) len#))) 
              (write-asig ~buffer ~b 0))
            ;(map-d ~buffer + b# ~buffer)
            (recur xs# (conj ret# ~x)))
          (recur xs# ret#)))
     ret#)))) 


(defn create-node 
  [& { :keys [channels] 
      :or {channels *nchnls*}
      }]
  { :audio-funcs (atom []) 
    :messages (ref [])
    :channels channels
   })

; currently will continue rendering even if afs are empty. need to have
; option to return nil when afs are empty, for the scenario of disk render.
; should add a *disk-render* flag to config and a default option here
; so that user can override behavior.
(defn node-processor
  "An audio-rate function that renders child audio-funcs and returns the 
  signals in an out-buffer."
 [node] 
 (let [out-buffer (double-array 
             (* *buffer-size* (:channels node)))
       node-afuncs (:audio-funcs node)
       ]
  (fn []
    (let [afs (run-audio-funcs @node-afuncs out-buffer)]
      (reset! node-afuncs afs) 
      out-buffer))))

(defn node-add-afunc
  "Adds an audio function to a node. Should not be called directly but rather be used via a message added to the node."
  [node afn]
  (when (not (.contains @(:audio-funcs node) afn))
    (swap! (:audio-funcs node) conj afn)))

(defn node-remove-afunc
  [node afn]
  )

(defn node-add-message
  [afn msg]
  )
