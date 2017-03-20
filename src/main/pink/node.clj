(ns pink.node
  "Nodes aggregate audio from other audio-rate functions. Nodes can contain other nodes. Each Node is wrapped in pink.util.shared so that the output of Node may be used by multiple other audio-functions within the same time period.
  
  In general, users will first call create-node to create a node map. node-processor will be used as the audio-rate function to add to an Engine, Node, or other audio-function.  
  "
  (:require [pink.config :refer [*nchnls* *buffer-size*]]
            [pink.event :refer :all]
            [pink.util :refer :all])
  (:import [pink.event Event]))


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

(defn- create-node-state
  [channels]
  { :funcs (atom []) 
   :pending-adds (atom [])
   :pending-removes (atom [])
   :status (atom nil)
   :channels channels
   })

(defn create-node 
  [& { :keys [channels] 
      :or {channels *nchnls*}
      }]
  (let [state (create-node-state channels)] 
    (reify 
      Node
      (node-add-func [this func]
        (swap! (:pending-adds state) conj func))
      (node-remove-func [this func]
        (swap! (:pending-removes state) conj func))
      (node-clear [this]
        (reset! (:status state) :clear))
      (node-empty?  [this]
        (and (empty? @(:funcs state)) 
             (empty? @(:pending-adds state))))
      (node-state [this] state)
      )))

(defn- update-funcs
  [v adds-atm removes-atm]
  (let [removes (drain-atom! removes-atm)]
    (filter #(< (.indexOf ^"clojure.lang.PersistentVector" removes %) 0)
           (concat-drain! v adds-atm))))

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
        node-afuncs (:funcs state)
        pending-adds (:pending-adds state)
        pending-removes (:pending-removes state)
        status (:status state)]
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
  (let [state (node-state node)
        node-funcs (:funcs state)
        pending-adds (:pending-adds state)
        pending-removes (:pending-removes state)
        status (:status state)]
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

(defn control-node
  "Creates a Node that also adheres to control function convention (0-arity IFn that
  returns true or nil)."
 []
  (let [state (create-node-state *nchnls*)
        node-funcs (:funcs state)
        pending-adds (:pending-adds state)
        pending-removes (:pending-removes state)
        status (:status state)] 
    (reify 
      Node
      (node-add-func [this func]
        (swap! (:pending-adds state) conj func))
      (node-remove-func [this func]
        (swap! (:pending-removes state) conj func))
      (node-clear [this]
        (reset! (:status state) :clear))
      (node-empty?  [this]
        (and (empty? @(:funcs state)) 
             (empty? @(:pending-adds state))))
      (node-state [this] state)
      clojure.lang.IFn
      (invoke [this]
        (if (= :clear @status)
          (do 
            (reset! node-funcs [])
            (reset! pending-adds [])
            (reset! pending-removes [])
            (reset! status nil))
          (let [newfns (process-cfuncs 
                         (update-funcs @node-funcs pending-adds pending-removes))] 
            (reset! node-funcs newfns))
          )))))

(defn audio-node
  "Creates a Node that also adheres to audio function convention (0-arity IFn that
  returns audio buffer or nil)."
  [& { :keys [channels] 
      :or {channels *nchnls*}
      }]
  (let [state (create-node-state channels)
        out-buffer (create-buffers (:channels state))
        node-funcs (:funcs state)
        pending-adds (:pending-adds state)
        pending-removes (:pending-removes state)
        status (:status state)] 
    (reify 
      Node
      (node-add-func [this func]
        (swap! (:pending-adds state) conj func))
      (node-remove-func [this func]
        (swap! (:pending-removes state) conj func))
      (node-clear [this]
        (reset! (:status state) :clear))
      (node-empty?  [this]
        (and (empty? @(:funcs state)) 
             (empty? @(:pending-adds state))))
      (node-state [this] state)

      clojure.lang.IFn
      (invoke [this]
        (clear-buffer out-buffer)
        (if (= :clear @status)
          (do 
            (reset! node-funcs [])
            (reset! pending-adds [])
            (reset! pending-removes [])
            (reset! status nil)) 
          (let [afs (run-node-funcs 
                      (update-funcs @node-funcs pending-adds pending-removes) 
                      out-buffer)]
            (reset! node-funcs afs)))
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


