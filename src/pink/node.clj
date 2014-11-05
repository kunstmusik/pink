(ns pink.node
  "Nodes aggregate audio from other audio-rate functions. Nodes can contain other nodes. Each Node is wrapped in pink.util.shared so that the output of Node may be used by multiple other audio-functions within the same time period.
  
  In general, users will first call create-node to create a node map. node-processor will be used as the audio-rate function to add to an Engine, Node, or other audio-function.  
  "
  (:require [pink.config :refer [*nchnls* *buffer-size*]]
            [pink.event :refer :all]
            [pink.util :refer :all])
  (:import [pink.event Event]))


(defn create-node 
  [& { :keys [channels] 
      :or {channels *nchnls*}
      }]
  { :funcs (atom []) 
    :pending-adds (atom [])
    :pending-removes (atom [])
    :status (atom nil)
    :channels channels
   })

(defn- update-funcs
  [v adds-atm removes-atm]
  (let [removes (drain-atom! removes-atm)]
    (filter #(< (.indexOf ^"clojure.lang.PersistentVector" removes %) 0)
           (concat-drain! v adds-atm))))

(defn run-node-funcs 
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

(defn- process-cfuncs
  [cfuncs]
  (loop [[x & xs] cfuncs
         ret []]
    (if x
      (if (try-func (x)) 
        (recur xs (conj ret x))
        (recur xs ret))
      ret)))

; currently will continue rendering even if afs are empty. need to have
; option to return nil when afs are empty, for the scenario of disk render.
; should add a *disk-render* flag to config and a default option here
; so that user can override behavior.
(defn node-processor
  "An audio-rate function that renders child funcs and returns the 
  signals in an out-buffer."
  [node] 
  (let [out-buffer (create-buffers (:channels node))
        node-afuncs (:funcs node)
        pending-adds (:pending-adds node)
        pending-removes (:pending-removes node)
        status (:status node)]
    (fn []
      (clear-buffer out-buffer)
      (if (= :clear @status)
        (do 
          (reset! node-afuncs [])
          (reset! pending-adds [])
          (reset! pending-removes [])
          (reset! status nil)) 
        (let [afs (run-node-funcs 
                    (update-funcs @node-afuncs pending-adds pending-removes) 
                    out-buffer)]
          (reset! node-afuncs afs)))
      out-buffer
      )))

(defn control-node-processor
  "Creates a control node processing functions that runs controls functions, 
  handling pending adds and removes, as well as filters out done functions."
  [node]
  (let [node-funcs (:funcs node)
        pending-adds (:pending-adds node)
        pending-removes (:pending-removes node)
        status (:status node)]
    (fn []
      (if (= :clear @status)
        (do 
          (reset! node-funcs [])
          (reset! pending-adds [])
          (reset! pending-removes [])
          (reset! status nil))
        (let [newfns (process-cfuncs 
                       (update-funcs @node-funcs pending-adds pending-removes))] 
          (reset! node-funcs newfns))
        ))))

(defn node-add-func
  "Adds an audio function to a node. Should not be called directly but rather be used via a message added to the node."
  [node afn]
  (swap! (:pending-adds node) conj afn))

(defn node-remove-func
  [node afn]
  (swap! (:pending-removes node) conj afn))

(defn node-clear
  [node]
  (reset! (:status node) :clear))

(defn node-empty?
  [node]
  (and (empty? @(:funcs node)) (empty? @(:pending-adds node))))

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
