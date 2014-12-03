(ns pink.event
  (:require [pink.util :refer [create-buffer drain-atom! try-func apply!*!]]
            [pink.config :refer [*tempo*]]  )
  (:import [java.util Collection PriorityQueue]))

(deftype Event [event-func ^double start event-args ]
  Object
  (toString [_]  (format "\t%s\t%s\t%s\n" event-func start event-args ))
  (hashCode [this] (System/identityHashCode this))
  (equals [this b] (identical? this b))

  Comparable
  (compareTo [this a] 
    (let [t1 (.start this)
          t2 (.start ^Event a)] 
     (compare t1 t2))))

(deftype EventList [^PriorityQueue events pending-events cur-beat
                    buffer-size sr tempo-atom]
  Object
  (toString [_]  (str events))
  (hashCode [this] (System/identityHashCode this))
  (equals [this b] (identical? this b)))

(defn event 
  "Create an Event object. Can either pass args as list or variadic args."
  ([f start] 
   (Event. f start []))
  ([f start args]
   (if (sequential? args)
     (Event. f start args) 
     (Event. f start [args]))) 
  ([f start x & args]
   (Event. f start (list* x args))))

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

  ([buffer-size sr] (event-list [] buffer-size sr))
  ([^Collection evts buffer-size sr] 
   (EventList. 
     (PriorityQueue. evts) (atom []) (atom 0.0) buffer-size sr (atom *tempo*))))

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

(defn event-list-get-tempo-atom
  [^EventList evtlst]
  (.tempo-atom evtlst))

(defn event-list-empty?
  [^EventList evtlst]
  (.isEmpty ^PriorityQueue (.events evtlst)))

(defn fire-event 
  "Evaluates event as delayed function application. Swallows exceptions and
  returns nil."
  [evt]
  (try-func (apply!*! (.event-func ^Event evt) 
                 (.event-args ^Event evt))))

(defn- merge-pending!
  "Merges pending-events with the PriorityQueue of known events. Adjusts start times of events to *tempo*."
  [^EventList evtlst]
  (let [pending (.pending-events evtlst)]
    (try 
      (when (not-empty @pending) 
        (let [new-events (drain-atom! pending)
              cur-beat (double @(.cur-beat evtlst))
              tempo (double @(.tempo-atom evtlst))
              timed-events 
              (map (fn [^Event a] (alter-event-time (+ cur-beat (.start a)) a)) 
                   new-events)] 
          (.addAll ^PriorityQueue (.events evtlst) timed-events))
        )
      (catch Exception e 
        (println "Error: Invalid pending events found!") 
        nil))))

(defn seconds->beats
  [^double seconds ^double tempo]
  (* seconds (/ tempo 60.0)))

(defn beats->seconds
  [^double beats ^double tempo]
  (* beats (/ 60.0 tempo)))

(defn event-list-tick!
  [^EventList evtlst] 
  (merge-pending! evtlst)
  (let [cur-beat (double @(.cur-beat evtlst))
        tempo (double @(.tempo-atom evtlst))
        time-adj (seconds->beats (/ (.buffer-size evtlst) (.sr evtlst)) tempo)
        end-time (+ cur-beat time-adj)
        events ^PriorityQueue (.events evtlst)]
    (binding [*tempo* tempo]    
      (loop [evt ^Event (.peek events)]
        (when (and evt (< (.start evt) end-time)) 
          (fire-event (.poll events))
          (recur (.peek events)))))
    (reset! (.cur-beat evtlst) end-time)))

(defn event-list-processor 
  "Returns a control-function that ticks through an event list"
  [^EventList evtlst]
  (fn ^doubles []
    (event-list-tick! evtlst)
    (not (.isEmpty ^PriorityQueue (.events evtlst)))))


