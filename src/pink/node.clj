(ns pink.node
  "Nodes are audio-rate functions that aggregate audio from other audio-rate functions. Nodes can contain other nodes. Each Node is wrapped in pink.util.shared so that the output of Node may be used by multiple other audio-functions within the same time period.
  
  In general, users will first call create-node to create a node map. node-processor will be used as the audio-rate function to add to an Engine, Node, or other audio-function.  
  "
  (:require [pink.config :refer [*nchnls* *buffer-size*]]
            [pink.util :refer [create-buffer create-buffers
                               fill map-d swapd! setd! getd 
                               arg mix-buffers]]))


(defmacro run-node-audio-funcs 
  [afs buffer]
  (let [x (gensym)
        b (gensym)]
   `(loop [[~x & xs#] ~afs 
          ret# []]
    (if ~x 
      (let [~b (~x)]
        (if ~b
          (do 
            (mix-buffers ~b ~buffer)
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
 (let [out-buffer (create-buffers (:channels node))
       node-afuncs (:audio-funcs node)]
  (fn []
    (let [afs (run-node-audio-funcs @node-afuncs out-buffer)]
      (reset! node-afuncs afs) 
      out-buffer))))

(defn node-add-afunc
  "Adds an audio function to a node. Should not be called directly but rather be used via a message added to the node."
  [node afn]
  (when (not (.contains ^"clojure.lang.PersistentVector" @(:audio-funcs node) afn))
    (swap! (:audio-funcs node) conj afn)))

(defn node-remove-afunc
  [node afn]
  )

(defn node-add-message
  [afn msg]
  )
