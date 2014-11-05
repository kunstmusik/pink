(ns pink.node
  "Nodes aggregate audio from other audio-rate functions. Nodes can contain other nodes. Each Node is wrapped in pink.util.shared so that the output of Node may be used by multiple other audio-functions within the same time period.
  
  In general, users will first call create-node to create a node map. node-processor will be used as the audio-rate function to add to an Engine, Node, or other audio-function.  
  "
  (:require [pink.config :refer [*nchnls* *buffer-size*]]
            [pink.event :refer :all]
            [pink.util :refer :all])
  (:import [pink.event Event]))

(defn run-node-audio-funcs 
  [afs buffer]
  (loop [[x & xs] afs 
         ret []]
    (if x 
      (let [b (try-func (x))]
        (if b
          (do 
            (mix-buffers b buffer)
            (recur xs (conj ret x)))
          (recur xs ret)))
      ret))) 

(defn create-node 
  [& { :keys [channels] 
      :or {channels *nchnls*}
      }]
  { :audio-funcs (atom []) 
    :pending-afuncs (atom [])
    :pending-remove-afuncs (atom [])
    :status (atom nil)
    :channels channels
   })

(defn- update-funcs
  [v adds-atm removes-atm]
  (let [removes (drain-atom! removes-atm)]
    (filter #(< (.indexOf ^"clojure.lang.PersistentVector" removes %) 0)
           (concat-drain! v adds-atm))))

; currently will continue rendering even if afs are empty. need to have
; option to return nil when afs are empty, for the scenario of disk render.
; should add a *disk-render* flag to config and a default option here
; so that user can override behavior.
(defn node-processor
  "An audio-rate function that renders child audio-funcs and returns the 
  signals in an out-buffer."
  [node] 
  (let [out-buffer (create-buffers (:channels node))
        node-afuncs (:audio-funcs node)
        pending-afuncs (:pending-afuncs node)
        pending-remove-afuncs (:pending-remove-afuncs node)
        status (:status node)]
    (fn []
      (clear-buffer out-buffer)
      (if (= :clear @status)
        (do 
          (reset! node-afuncs [])
          (reset! pending-afuncs [])
          (reset! pending-remove-afuncs [])
          (reset! status nil)) 
        (let [afs (run-node-audio-funcs 
                    (update-funcs @node-afuncs pending-afuncs pending-remove-afuncs) 
                    out-buffer)]
          (reset! node-afuncs afs)))
      out-buffer
      )))

(defn node-add-afunc
  "Adds an audio function to a node. Should not be called directly but rather be used via a message added to the node."
  [node afn]
  (swap! (:pending-afuncs node) conj afn))

(defn node-remove-afunc
  [node afn]
  (swap! (:pending-remove-afuncs node) conj afn))


;; Event functions dealing with nodes

(defn fire-node-event 
  "create an instance of an audio function and adds to the node" 
  [node evt]  
  (when-let [afunc (fire-event evt)] 
    (node-add-afunc node afunc)))

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
