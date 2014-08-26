(ns pink.event
  (:require [pink.engine :refer :all]
            [pink.node :refer [node-add-afunc]]
            [pink.util :refer [create-buffer]]
            [pink.config :refer [*buffer-size* *sr*]]  )
  (:import [java.util List]))

(deftype Event [event-func ^Double start event-args ]
  Object
  (toString [this]  (format "\t%s\t%s\t%s\n" event-func start event-args )) 
  Comparable
  (compareTo [this a] (- (.start this) (.start ^Event a))))

(defn event 
  "Create an Event object. Can either pass args as list or variadic args."
  ([f start args]
   (if (sequential? args)
     (Event. f start args) 
     (Event. f start [args]))) 
  ([f start x & args]
   (Event. f start (list* x args)))
  )

(defn events [f & args]
  (map #(apply event f %) args))


(defn event-list
  "Creates an EventList. 

  EventList's its own internal time and fires off events whose start times have
  been met.  Event have no notion of duration. An event may do things like 
  schedule an audio function to be added to an engine's performance list, force
  turning off an audio function, and so on."

  ([] (event-list []))
  ([evts] 
   {:events (ref (sort-by #(.start ^Event %) evts))       
    :cur-buffer (atom 0)
    }))

(defn event-list-add 
  "Add an event to an event list"
  [evtlst ^Event evt] 
  (when (< (.indexOf ^List @(:events evtlst) evt) 0)
    (dosync
      (alter (:events evtlst) 
             (fn [a] (sort-by #(.start ^Event %) (conj a evt))))))
  evtlst)

(defn event-list-remove 
  "remove an event from the event list"
  [evtlst evt] 
  (do
    (dosync
      (alter (:events evtlst) (fn [a] (remove #(= % evt) a)))) 
    evtlst))

(defn- get-events! 
  "alters an event list, returns events that are scheduled to be fired, updates 
  events in the event list"
  [evtlst cur-time]
  (dosync 
    (let [events (:events evtlst)
          [ready pending] (split-with #(<=  (.start ^Event %) cur-time) @events)]
      (ref-set events (if pending pending [])) 
      ready)))

(defn- fire-event [evt]
  (apply (.event-func ^Event evt) 
                 (.event-args ^Event evt)))

(defn event-list-tick [evtlst] 
  (let [cur-buffer (:cur-buffer evtlst)
        cur-time (/ (* @cur-buffer *buffer-size*) *sr*)
        ready-events (get-events! evtlst cur-time)]
    (loop [[a & b] ready-events]
      (when a
        (do 
          (fire-event a)
          (recur b))))
    (swap! cur-buffer inc)))

;; Events functions dealing with audio engines

(defn fire-engine-event 
  "create an instance of an audio function and adds to the engine" 
  [eng f & args]  
  (engine-add-afunc eng (apply f args)))

(defn wrap-engine-event [eng ^Event evt]
  (event fire-engine-event 
         (.start evt)
         (list* eng (.event-func evt) (.event-args evt))))

(defn engine-events 
  "Takes an engine and series of events, wrapping the events as engine-events.
  If single arg given, assumes it is a list of events."
  ([eng args]
   (if (sequential? args)
     (event-list (map #(wrap-engine-event eng %) args))    
     (event-list (map #(wrap-engine-event eng %) [args]))))
  ([eng x & args]
   (engine-events eng (list* x args))))


(defn event-list-processor [evtlst]
  (let [buf (create-buffer)]
    (fn ^doubles []
      (event-list-tick evtlst)
      (if (empty? @(:events evtlst))
        nil
        buf))))

;; Event functiosn dealing with nodes


(defn fire-node-event 
  "create an instance of an audio function and adds to the engine" 
  [node f & args]  
  (node-add-afunc node (apply f args)))

(defn wrap-node-event [eng ^Event evt]
  (event fire-node-event 
         (.start evt)
         (list* eng (.event-func evt) (.event-args evt))))

(defn node-events 
  "Takes a node and series of events, wrapping the events as node-events.
  If single arg given, assumes it is a list of events."
  ([node args]
   (if (sequential? args)
     (event-list (map #(wrap-node-event node %) args))    
     (event-list (map #(wrap-node-event node %) [args]))))
  ([node x & args]
   (node-events node (list* x args))))

(comment

  (defn test-event-list [evtlst]
    (let [wait (* 1000 (/ *buffer-size* *sr*))]
      (loop []
        (event-list-tick evtlst)

        (when (> (count @(:events evtlst)) 0)
          (Thread/sleep 1) 
          (recur)
          ))))


  (defn test-func [a b] (println "test-func fired" a b))

  ;(events test-func [1 2 3] [4 5 6])

  (def test-note (event test-func 0.0 1.0 440.0))
  (def test-note-dupe (event test-func 0.0 1.0 440.0))
  (def test-note2 (event test-func 0.0 1.0 220.0))
  (def test-note3 (event test-func 2.0 1.5 110.0))
  (print (.start test-note3))

  (def evtlst (event-list [test-note]))
  (event-list-add evtlst test-note3)

  (print evtlst)

  (.start (Thread. ^Runnable (partial test-event-list evtlst))) 



  (def eng (engine-create))

  (def eng-events 
    (engine-events eng
                   (event test-func 0.0 1.5 110.0) 
                   (event test-func 0.0 1.5 120.0) 
                   (event test-func 1.0 1.5 130.0) 
                   (event test-func 2.0 1.5 140.0)))
  (print eng-events)
  (print (count @(:events eng-events)))
  (.start (Thread. ^Runnable (partial test-event-list eng-events))) 

  (print eng)

  ;(event-list-remove evtlst test-note)
  ;(event-list-tick evtlst)


  )
