(ns pink.event
  (:require [pink.util :refer [create-buffer drain-atom! try-func]]
            [pink.config :refer [*buffer-size* *sr*]]  )
  (:import [java.util List PriorityQueue]))

(deftype Event [event-func ^double start event-args ]
  Object
  (toString [this]  (format "\t%s\t%s\t%s\n" event-func start event-args )) 
  (hashCode [this] (System/identityHashCode this))
  (equals [this b] (identical? this b))

  Comparable
  (compareTo [this a] 
    (let [t1 (.start this)
          t2 (.start ^Event a)] 
     (cond (> t1 t2) 1
           (< t1 t2) -1
           :else 0))))

(deftype EventList [^PriorityQueue events pending-events cur-buffer ]
  Object
  (toString [this]  (str events)) 
  (hashCode [this] (System/identityHashCode this))
  (equals [this b] (identical? this b)))

(defn event 
  "Create an Event object. Can either pass args as list or variadic args."
  ([f start args]
   (if (sequential? args)
     (Event. f start args) 
     (Event. f start [args]))) 
  ([f start x & args]
   (Event. f start (list* x args)))
  )

(defn wrap-event 
  "Wraps an event with other top-level functions."
  [f pre-args ^Event evt ]
  (event f (.start evt) (conj pre-args evt)))

(defn alter-event-time
  "Utility function to create a new Event using the same values as the
  passed-in event and new start time."
  [start ^Event evt]
  (event (.event-func evt) start (.event-args evt)))

(defn events [f & args]
  (map #(apply event f %) args))

(defn event-list
  "Creates an EventList. 

  EventLists maintain their own internal time and fire off events whose start
  times have been met.  Events have no notion of duration. An event may do
  things like schedule an audio function to be added to an engine's
  performance list, force turning off an audio function, and so on."

  ([] (event-list []))
  ([^List evts] 
   (EventList. (PriorityQueue. evts) (atom []) (atom 0))))

(defn event-list-add 
  "Add an event or events to an event list"
  [^EventList evtlst evts] 
  (let [pending (.pending-events evtlst)]
    (cond 
      (sequential? evts) 
        (swap! pending concat evts) 
      (:events evts)
        (swap! pending concat (.events ^EventList evts)) 
      (instance? Event evts) 
        (swap! pending conj evts) 
      :else
      (throw (Exception. (str "Unexpected event: " evts)))))
  nil)

(defn event-list-clear
  "Clear all events from an EventList"
  [^EventList evtlst]
  (.clear ^PriorityQueue (.events evtlst)))

(defn event-list-remove 
  "remove an event from the event list"
  [^EventList evtlst evt] 

  ; this needs to be done using a pending-removals list 
  
  )

(defn fire-event 
  "Evaluates event as delayed function application. Swallows exceptions and
  returns nil."
  [evt]
  (try-func (apply (.event-func ^Event evt) 
                 (.event-args ^Event evt))))

(defn- merge-pending!
  "Merges pending-events with the PriorityQueue of known events."
  [^EventList evtlst]
  (let [pending (.pending-events evtlst)]
    (when (not-empty @pending) 
      (try 
        (let [new-events (drain-atom! pending)
            cur-buffer (.cur-buffer evtlst)
            cur-time (/ (* (long @cur-buffer) (long *buffer-size*)) (double *sr*))
            timed-events 
            (map (fn [^Event a] (alter-event-time (+ cur-time (.start a)) a)) 
                 new-events)] 
        (.addAll ^PriorityQueue (.events evtlst) timed-events))
        (catch Exception e 
          (println "Error: Invalid pending events found!") 
          nil)))))

(defn event-list-tick!
  [^EventList evtlst] 
  (merge-pending! evtlst)
  (let [cur-buffer (.cur-buffer evtlst)
        cur-time (/ (* (+ 1 (long @cur-buffer)) (long *buffer-size*)) (double *sr*))
        events ^PriorityQueue (.events evtlst)]
    (loop [evt ^Event (.peek events)]
      (when (and evt (< (.start evt) cur-time)) 
          (fire-event (.poll events))
          (recur (.peek events))))
    (swap! cur-buffer inc)))

(defn event-list-processor 
  "Returns a control-function that ticks through an event list"
  [^EventList evtlst]
  (fn ^doubles []
    (event-list-tick! evtlst)
    (not (.isEmpty ^PriorityQueue (.events evtlst)))))

(comment

  (defn test-event-list [evtlst]
    (let [wait (* 1000 (/ *buffer-size* *sr*))]
      (loop []
        (event-list-tick evtlst)

        (when (> (count @(.events evtlst)) 0)
          (Thread/sleep 1) 
          (recur)
          ))))


  (defn test-func [a b] (println "test-func fired" a b))

  ;(events test-func [1 2 3] [4 5 6])

  (def test-note (event test-func 0.0 1.0 440.0))
  (def test-note-dupe (event test-func 0.0 1.0 440.0))
  (def test-note2 (event test-func 0.5 1.0 220.0))
  (def test-note3 (event test-func 2.0 1.5 110.0))
  (print (.start test-note3))
  (def evtlst (event-list [test-note test-note-dupe test-note2 test-note3]))
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
  (print (count @(.events eng-events)))
  (.start (Thread. ^Runnable (partial test-event-list eng-events))) 

  (print eng)

  ;(event-list-remove evtlst test-note)
  ;(event-list-tick evtlst)


  )
