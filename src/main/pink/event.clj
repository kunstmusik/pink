(ns pink.event
  (:require [pink.util :refer [create-buffer drain-atom! apply!*!]]
            [pink.config :refer [*tempo* *beat*]]  )
  (:import [java.util Collection PriorityQueue]
           [java.util.concurrent ArrayBlockingQueue]
           ))

;; Ensure unchecked math used for this namespace
(set! *unchecked-math* :warn-on-boxed)

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

(definterface IEventList
  (^void setEventProcFn 
    [proc-fn] 
    "Set event processing function. proc-fn should be an arity 2 function with
    input arguments of type double and Event. First argument will be the
    current beat time of the event list and the second argument will be the
    current Event to process.")
  (getEventProcFn 
    [] "Returns the current event processing function for the EventList.")
  (^double getCurBeat [] "Return current beat time")
  (^void setCurBeat [^double beat] "Set current beat time")
  (^double getTempo [] "Return tempo")
  (^void setTempo [^double beat] "Set tempo"))

(deftype EventList [^PriorityQueue events ^ArrayBlockingQueue pending-events 
                    ^ArrayBlockingQueue temp-pending
                    ^:unsynchronized-mutable ^double cur-beat 
                    ^long buffer-size ^long sr 
                    ^:unsynchronized-mutable ^double tempo 
                    ^:unsynchronized-mutable event-proc-fn]
  IEventList
  (^void setEventProcFn [_ proc-fn] (set! event-proc-fn proc-fn) nil)
  (getEventProcFn [_] event-proc-fn)
  (^double getCurBeat [_] cur-beat)
  (^void setCurBeat [_ ^double beat] (set! cur-beat beat) nil)
  (^double getTempo [_] tempo)
  (^void setTempo [_ ^double new-tempo] (set! tempo new-tempo) nil) 

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
  [^double start ^Event evt]
  (event (.event-func evt) start (.event-args evt)))

(defn wrap-relative-start
  [^double cur-beat ^Event a] 
  (alter-event-time (+ cur-beat (.start a)) a)) 


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
     (PriorityQueue. evts) (ArrayBlockingQueue. 32768) (ArrayBlockingQueue. 32768)
     0.0 buffer-size sr (double *tempo*) 
     wrap-relative-start)))

(defn event-list-add 
  "Add an event or events to an event list"
  [^EventList evtlst evts] 
  (let [^ArrayBlockingQueue pending (.pending-events evtlst)]
    (cond 
      (sequential? evts) 
      (.addAll pending evts)
      (:events evts)
      (.addAll pending (.events ^EventList evts)) 
      (instance? Event evts) 
      (.add pending evts)
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

(defn event-list-empty?
  [^EventList evtlst]
  (.isEmpty ^PriorityQueue (.events evtlst)))

(defn fire-event 
  "Evaluates event as delayed function application. Swallows exceptions and
  returns nil."
  [evt]
  (try 
    (apply!*! (.event-func ^Event evt) 
                 (.event-args ^Event evt))
    (catch Throwable t
      (.printStackTrace t)
      nil)))

(defn- merge-pending!
  "Merges pending-events with the PriorityQueue of known events. Event start
  times are processed relative to the EventList's cur-beat."
  [^EventList evtlst]
  (let [^ArrayBlockingQueue pending (.pending-events evtlst)
        ^ArrayBlockingQueue temp-pending (.temp-pending evtlst)
        ^PriorityQueue active-events (.events evtlst)]
    (.drainTo pending temp-pending)
    (try 
      (when (> (.size temp-pending) 0) 
        (let [cur-beat (.getCurBeat evtlst) 
              tempo (.getTempo evtlst) 
              event-proc-fn (.getEventProcFn evtlst)] 
          (doseq [x temp-pending]
            (.add active-events (event-proc-fn cur-beat x)))
          (.clear temp-pending)))
      (catch Exception e 
        (println "Error: Invalid pending events found!") 
        nil))))

(defn seconds->beats
  ^double [^double seconds ^double tempo]
  (* seconds (/ tempo 60.0)))

(defn beats->seconds
  ^double [^double beats ^double tempo]
  (* beats (/ 60.0 tempo)))

(defn- proc-events
  [^PriorityQueue events ^double cur-beat
   ^double end-time]
  (loop [evt ^Event (.peek events)]
        (when evt
          (if (< (.start evt) cur-beat)
            (do 
              (.poll events) 
              (recur (.peek events)))
            (when (< (.start evt) end-time) 
              (fire-event (.poll events))
              (recur (.peek events)))))))

(defn event-list-tick!
  [^EventList evtlst] 
  (merge-pending! evtlst)
  (let [cur-beat (.getCurBeat evtlst) 
        tempo (.getTempo evtlst) 
        time-adj (seconds->beats 
                   (/ (double (.buffer-size evtlst)) 
                       (double (.sr evtlst))) 
                   tempo)
        end-time (+ cur-beat time-adj)
        events ^PriorityQueue (.events evtlst)]
    ;; setting curbeat before binding so that 
    ;; binding is in tail-position and therefore does
    ;; not produce closure by Clojure Compiler
    (.setCurBeat evtlst end-time)
    (binding [*tempo* tempo
              *beat* cur-beat]    
      (proc-events events cur-beat end-time))
    ))

(defn event-list-processor 
  "Returns a control-function that ticks through an event list"
  [^EventList evtlst]
  (fn ^doubles []
    (event-list-tick! evtlst)
    (not (.isEmpty ^PriorityQueue (.events evtlst)))))


(defn use-absolute-time! 
  "Set EventList to insert new events without modification
  to start times."
  [^EventList evtlst]
  (.setEventProcFn evtlst (fn [_ evt] evt)) )


(defn use-relative-time! 
  "Set EventList to insert new events processing their start times as relative
  to the cur-beat. (This is the default behavior of EventList.)"
  [^EventList evtlst]
  (.setEventProcFn evtlst wrap-relative-start))
