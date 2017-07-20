(ns pink.node
  "Nodes aggregate audio from other audio-rate functions. Nodes can contain other nodes. Each Node is wrapped in pink.util.shared so that the output of Node may be used by multiple other audio-functions within the same time period.
  
  In general, users will first call create-node to create a node map. node-processor will be used as the audio-rate function to add to an Engine, Node, or other audio-function.  
  "
  (:require [pink.config :refer [*nchnls* *buffer-size*]]
            [pink.event :refer :all]
            [pink.util :refer :all]
            [pink.dynamics :refer [db->amp]])
  (:import [pink.event Event]
           [pink.node MessageBuffer Message]
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
  (set-gain! [n gain-val] "Set gain [0,1] to apply to signal"))

(defprotocol StereoMixerNode
  (set-pan! [n pan-val] "Set pan [-1,1] to apply to signal"))


;; NODE

(defn- create-node-state
  ([channels] (create-node-state channels 512))
  ([channels max-messages]
   { :funcs (atom []) 
    :messages (MessageBuffer. max-messages)
    :channels channels
    }))

(defn create-node 
  [& { :keys [channels max-messages] 
      :or {channels *nchnls*
           max-messages 512 }}]
  (let [state (create-node-state channels max-messages)
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
        (.isEmpty mbuf))
      (node-state [this] state)
      )))

(defn- process-messages!
  [v ^MessageBuffer mbuf]
  (let [start (.getReadStart mbuf) 
        end (.getReadEnd mbuf)
        capacity (.getCapacity mbuf)
        adj-end (long (if (< end start) (+ end capacity) end))]
    (if (< start adj-end)
     (loop [indx start 
            out v]
       (if (< indx adj-end)
         (let [^Message msg (.getMessage mbuf (rem indx capacity))
               msgType (.getMsgType msg)
               msgMsg (.getMsg msg)]
            (.reset msg)
            (case msgType
              :add 
              (recur (inc indx) (conj out msgMsg))
              :clear
              (recur (inc indx) [])
              (recur (inc indx) v)))
         (do
           (.setReadStart mbuf end)
           out)
         ))
     v)))

(defn run-node-funcs 
  [afs buffer]
  (loop [[x & xs] afs 
         ret (transient [])]
    (if x 
      (let [b (try-func (x))]
        (if b
          (do 
            (mix-buffers b buffer)
            (recur xs (conj! ret x)))
          (recur xs ret)))
      (persistent! ret)))) 

(defn- process-cfuncs
  [cfuncs]
  (loop [[x & xs] cfuncs
         ret (transient [])]
    (if x
      (if (try-func (x)) 
        (recur xs (conj! ret x))
        (recur xs ret))
      (persistent! ret))))

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
        node-afuncs (:funcs state)
        status (:status state)]
    (fn []
      (clear-buffer out-buffer)
      (let [afs (->
                  (process-messages! @node-afuncs mbuf)
                  (run-node-funcs out-buffer))]
        (reset! node-afuncs afs))
      out-buffer
      )))

(defn control-node-processor
  "Creates a control node processing functions that runs controls functions, 
  handling pending adds and removes, as well as filters out done functions."
  [node]
  (let [state (node-state node)
        node-funcs (:funcs state)
        ^MessageBuffer mbuf (:messages state)]
    (fn []
      (let [newfns (-> 
                     (process-messages! @node-funcs mbuf)
                     (process-cfuncs))] 
        (reset! node-funcs newfns)))))

(defn control-node
  "Creates a Node that also adheres to control function convention (0-arity IFn that
  returns true or nil)."
  ([& { :keys [channels max-messages] 
      :or {channels *nchnls* 
           max-messages 512} }]
   (let [state (create-node-state *nchnls* max-messages)
         node-funcs (:funcs state)
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
         (.isEmpty mbuf))
       (node-state [this] state)
       clojure.lang.IFn
       (invoke [this]
         (let [newfns (-> 
                        (process-messages! @node-funcs mbuf)
                        (process-cfuncs))] 
           (reset! node-funcs newfns)))))))

(defn audio-node
  "Creates a Node that also adheres to audio function convention (0-arity IFn that
  returns audio buffer or nil)."
  [& { :keys [channels max-messages] 
      :or {channels *nchnls* 
           max-messages 512} }]
  (let [state (create-node-state channels)
        out-buffer (create-buffers (:channels state))
        ^MessageBuffer mbuf (:messages state)
        node-funcs (:funcs state)] 
    (reify 
      Node
      (node-add-func [this func]
        (.postMessage mbuf :add func))
      (node-remove-func [this func]
        (.postMessage mbuf :remove func))
      (node-clear [this]
        (.postMessage mbuf :clear nil))
      (node-empty?  [this]
        (.isEmpty mbuf))
      (node-state [this] state)

      clojure.lang.IFn
      (invoke [this]
        (clear-buffer out-buffer)
        (let [afs (->
                    (process-messages! @node-funcs mbuf)
                    (run-node-funcs out-buffer))]
          (reset! node-funcs afs))
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
         node-funcs (:funcs state)
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
         (.isEmpty mbuf))
       (node-state [this] state)

       GainNode
       (set-gain! [this gain-val] 
         (aset gain 0 (double gain-val)))

       StereoMixerNode
       (set-pan! [this pan-val] 
         (aset pan 0 (double pan-val)))

       clojure.lang.IFn
       (invoke [this]
         (clear-buffer mono-buffer)
         (let [afs (->
                     (process-messages! @node-funcs mbuf)
                     (run-node-funcs mono-buffer))
               ksmps (long *buffer-size*)
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
                 (recur (+ indx 1)))))
           (reset! node-funcs afs))
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
        node-funcs (:funcs state)
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
        (.isEmpty mbuf))
      (node-state [this] state)

      GainNode
      (set-gain! [this gain-val] 
        (aset gain 0 (double gain-val)))

      clojure.lang.IFn
      (invoke [this]
        (clear-buffer out-buffer)
        (let [afs (->
                    (process-messages! @node-funcs mbuf)
                    (run-node-funcs out-buffer))
              ksmps (long *buffer-size*)
              g (aget gain 0)]
          (loop [indx 0]
            (loop [indx 0]
              (when (< indx ksmps)
                (aset left indx (* g (aget left indx))) 
                (aset right indx (* g (aget right indx))) 
                (recur (+ indx 1)))))
          (reset! node-funcs afs))
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


