(ns pink.node
  "Nodes aggregate audio from other audio-rate functions. Nodes can contain other nodes. Each Node is wrapped in pink.util.shared so that the output of Node may be used by multiple other audio-functions within the same time period.
  
  In general, users will first call create-node to create a node map. node-processor will be used as the audio-rate function to add to an Engine, Node, or other audio-function.  
  "
  (:require [pink.config :refer [*nchnls* *buffer-size*]]
            [pink.event :refer :all]
            [pink.util :refer :all]
            [pink.dynamics :refer [db->amp]])
  (:import [pink.event Event]
           [pink.node MessageBuffer Message IFnList]
           [clojure.lang IFn]
           [java.util ArrayList]
           ))

;; Ensure unchecked math used for this namespace
(set! *unchecked-math* :warn-on-boxed)

(defprotocol Node
  (node-add-func 
    [n func] 
    "Add function to pending adds list") 
  (node-remove-func 
    [n func]
    "Add func to pending removes list") 
  (node-clear 
    [n] 
    "Clear out all active and pending funcs") 
  (node-empty? 
    [n] 
    "Checks whether active and pending add lists are empty")
  (node-state 
    [n] 
    "Returns state map for Node"))

(defprotocol GainNode
  (set-gain! [n gain-val] "Set gain [0,1] to apply to signal")
  (get-gain [n] "Get gain value.")
  )

(defprotocol StereoMixerNode
  (set-pan! [n pan-val] "Set pan [-1,1] to apply to signal")
  (get-pan [n] "Get pan value."))


;; NODE

(defn- create-node-state
  ([channels] (create-node-state channels 512))
  ([channels max-messages]
   {:funcs (IFnList.) 
    :messages (MessageBuffer. max-messages)
    :channels channels
    }))

(defn create-node 
  [& { :keys [channels max-messages] 
      :or {channels *nchnls*
           max-messages 512 }}]
  (let [state (create-node-state channels max-messages)
        ^MessageBuffer mbuf (:messages state)
        ^IFnList node-funcs (:funcs state)] 
    (reify 
      Node
      (node-add-func [this func]
        (.postMessage mbuf :add func))
      (node-remove-func [this func]
        (.postMessage mbuf :remove func))
      (node-clear [this]
        (.postMessage mbuf :clear nil))
      (node-empty?  [this]
        (and (.isEmpty mbuf)
             (.isEmpty node-funcs)))
      (node-state [this] state)
      )))

(defn- process-messages!
  [^IFnList node-funcs ^MessageBuffer mbuf]
  (let [start (.getReadStart mbuf) 
        end (.getReadEnd mbuf)
        capacity (.getCapacity mbuf)
        adj-end (long (if (< end start) (+ end capacity) end))
        ^ArrayList active-funcs (.getActiveList node-funcs) ]
    (when (< start adj-end)
     (loop [indx start]
       (if (< indx adj-end)
         (let [^Message msg (.getMessage mbuf (rem indx capacity))
               msgType (.getMsgType msg)
               msgMsg (.getMsg msg)]
            (.reset msg)
            (case msgType
              :add 
              (.add active-funcs msgMsg)
              :remove
              (.remove active-funcs msgMsg)
              :clear
              (.clear active-funcs) 
              (println "Error: Unknown msgType found for node: " msgType))
            (recur (inc indx)))
         (.setReadStart mbuf end))))))

(defn run-afuncs!
  [^IFnList afs buffer]
  (let [^ArrayList active-funcs (.getActiveList afs)
        size (.size active-funcs)]
    (when (> size 0)
      (loop [i 0]
        (when (< i size)
          (let [^IFn x (.get active-funcs i) 
                b (try-func x)]
            (when b
              (mix-buffers b buffer)
              (.putBack afs x)))
          (recur (+ 1 i))))
      (.swap afs)
      ))) 

(defn- run-cfuncs!
  [^IFnList cfuncs]
  (let [^ArrayList active-funcs (.getActiveList cfuncs)
        size (.size active-funcs)]
    (when (> size 0)
      (loop [i 0]
        (when (< i size)
          (let [^IFn x (.get active-funcs i)]
            (when (try-func x) 
              (.putBack cfuncs x)))
          (recur (+ 1 i))))
      (.swap cfuncs))))

; currently will continue rendering even if afs are empty. need to have
; option to return nil when afs are empty, for the scenario of disk render.
; should add a *disk-render* flag to config and a default option here
; so that user can override behavior.
(defn node-processor
  "An audio-rate function that renders child funcs and returns the 
  signals in an out-buffer."
  [node] 
  (let [state (node-state node)
        out-buffer (create-buffers (:channels state))
        ^MessageBuffer mbuf (:messages state)
        ^IFnList node-afuncs (:funcs state)
        status (:status state)]
    (fn []
      (clear-buffer out-buffer)
      (process-messages! node-afuncs mbuf)
      (run-afuncs! node-afuncs out-buffer)
      out-buffer)))

(defn control-node-processor
  "Creates a control node processing functions that runs controls functions, 
  handling pending adds and removes, as well as filters out done functions."
  [node]
  (let [state (node-state node)
        ^IFnList node-funcs (:funcs state)
        ^MessageBuffer mbuf (:messages state)]
    (fn []
      (process-messages! node-funcs mbuf)
      (run-cfuncs! node-funcs))))

(defn control-node
  "Creates a Node that also adheres to control function convention (0-arity IFn that
  returns true or nil)."
  ([& { :keys [channels max-messages] 
      :or {channels *nchnls* 
           max-messages 512} }]
   (let [state (create-node-state *nchnls* max-messages)
         ^IFnList node-funcs (:funcs state)
         ^MessageBuffer mbuf (:messages state)]
     (reify 
       Node
       (node-add-func [this func]
         (.postMessage mbuf :add func))
       (node-remove-func [this func]
         (.postMessage mbuf :remove func))
       (node-clear [this]
         (.postMessage mbuf :clear nil))
       (node-empty?  [this]
         (and (.isEmpty mbuf)
              (.isEmpty node-funcs)))
       (node-state [this] state)
       clojure.lang.IFn
       (invoke [this]
         (process-messages! node-funcs mbuf)
         (run-cfuncs! node-funcs))))))

(defn audio-node
  "Creates a Node that also adheres to audio function convention (0-arity IFn that
  returns audio buffer or nil)."
  [& { :keys [channels max-messages] 
      :or {channels *nchnls* 
           max-messages 512} }]
  (let [state (create-node-state channels)
        out-buffer (create-buffers (:channels state))
        ^MessageBuffer mbuf (:messages state)
        ^IFnList node-funcs (:funcs state)] 
    (reify 
      Node
      (node-add-func [this func]
        (.postMessage mbuf :add func))
      (node-remove-func [this func]
        (.postMessage mbuf :remove func))
      (node-clear [this]
        (.postMessage mbuf :clear nil))
      (node-empty?  [this]
        (and (.isEmpty mbuf)
             (.isEmpty node-funcs)))
      (node-state [this] state)

      clojure.lang.IFn
      (invoke [this]
        (clear-buffer out-buffer)
        (process-messages! node-funcs mbuf)
        (run-afuncs! node-funcs out-buffer)
        out-buffer))))

(defn mixer-node
  "Creates an audio node that accepts mono-signal audio functions. mixer-node
  has properties for gain and panning which will be applied to all generated signals.
  Output is a stereo signal."
  [& { :keys [ max-messages] 
      :or {max-messages 512} }]   
   (let [state (create-node-state 1 max-messages)
         mono-buffer (create-buffer)
         ^doubles left (create-buffer)
         ^doubles right (create-buffer)
         out-buffer (into-array [left right])
         ^MessageBuffer mbuf (:messages state)
         ^IFnList node-funcs (:funcs state)
         ^doubles pan (double-array 1 0.0)
         ^doubles gain (double-array 1 1.0)
         PI2 (/ Math/PI 2)] 
     (reify 
       Node
       (node-add-func [this func]
         (.postMessage mbuf :add func))
       (node-remove-func [this func]
         (.postMessage mbuf :remove func))
       (node-clear [this]
         (.postMessage mbuf :clear nil))
       (node-empty?  [this]
         (and (.isEmpty mbuf)
              (.isEmpty node-funcs)))
       (node-state [this] state)

       GainNode
       (set-gain! [this gain-val] 
         (aset gain 0 (double gain-val))
         nil)
       (get-gain [this] 
         (aget gain 0))

       StereoMixerNode
       (set-pan! [this pan-val] 
         (aset pan 0 (double pan-val))
         nil)
       (get-pan [this] 
         (aget pan 0))

       clojure.lang.IFn
       (invoke [this]
         (clear-buffer mono-buffer)
         (process-messages! node-funcs mbuf)
         (run-afuncs! node-funcs mono-buffer) 
         (let [ksmps (long *buffer-size*)
               g (aget gain 0)
               p (aget pan 0)
               new-loc-v (+ 0.5 (* 0.5 p))
               new-l (db->amp (* 20 (Math/log (Math/cos (* PI2 new-loc-v )))))
               new-r (db->amp (* 20 (Math/log (Math/sin (* PI2 new-loc-v )))))]
           (loop [indx 0]
             (when (< indx ksmps)
               (let [samp (* g (aget mono-buffer indx))]
                 (aset left indx (* new-l samp)) 
                 (aset right indx (* new-r samp)) 
                 (recur (+ indx 1))))))
         out-buffer))))

(defn gain-node
  "Creates an audio node that accepts stereo-signal audio functions. gain-node
  sums the generated signals from the audio functions, multiplies by gain, and
  outputs the stereo-signal output."
  [& { :keys [channels max-messages] 
      :or {channels *nchnls* 
           max-messages 512} }]
  (let [state (create-node-state 2 max-messages)
        ^doubles left (create-buffer)
        ^doubles right (create-buffer)
        out-buffer (into-array [left right])
        ^IFnList node-funcs (:funcs state)
        ^MessageBuffer mbuf (:messages state)
        ^doubles gain (double-array 1 1.0)] 
    (reify 
      Node
      (node-add-func [this func]
        (.postMessage mbuf :add func))
      (node-remove-func [this func]
        (.postMessage mbuf :remove func))
      (node-clear [this]
        (.postMessage mbuf :clear nil))
      (node-empty?  [this]
        (and (.isEmpty mbuf)
             (.isEmpty node-funcs)))
      (node-state [this] state)

      GainNode
      (set-gain! [this gain-val] 
        (aset gain 0 (double gain-val))
        nil)
      (get-gain [this] 
        (aget gain 0))

      clojure.lang.IFn
      (invoke [this]
        (clear-buffer out-buffer)
        (process-messages! node-funcs mbuf)
        (run-afuncs! node-funcs out-buffer) 
        (let [ksmps (long *buffer-size*)
              g (aget gain 0)]
          (loop [indx 0]
            (loop [indx 0]
              (when (< indx ksmps)
                (aset left indx (* g (aget left indx))) 
                (aset right indx (* g (aget right indx))) 
                (recur (+ indx 1))))))
        out-buffer))))

;; Event functions dealing with nodes

(defn fire-node-event 
  "create an instance of an audio function and adds to the node" 
  [node evt]  
  (when-let [afunc (fire-event evt)] 
    (node-add-func node afunc)))

(defn wrap-node-event [node ^Event evt]
  (wrap-event fire-node-event [node] evt))

(defn node-events 
  "Takes a node and series of events, wrapping the events as node-events.
  If single arg given, assumes it is a list of events."
  ([node args]
   (if (sequential? args)
     (map #(wrap-node-event node %) args)    
     (map #(wrap-node-event node %) [args])))
  ([node x & args]
   (node-events node (list* x args))))


