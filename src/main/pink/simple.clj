(ns 
  ^{:doc "Simple interface for single-engine projects"
   :author "Steven Yi"}
  pink.simple
  (:require [pink.engine :refer :all]
            [pink.util :refer [with-duration apply!*!]]
            [pink.event :refer [event event-list-beat-time use-absolute-time!]])
  (:import [pink.engine Engine]
           [pink.event EventList]))

;; Ensure unchecked math used for this namespace
(set! *unchecked-math* :warn-on-boxed)

(def engine (engine-create :nchnls 2))

(defn start-engine 
  "Starts the global pink.simple engine."
  []
  (engine-start engine))

(defn stop-engine 
  "Stops the global pink.simple engine."
  []
  (engine-stop engine))

(defn clear-engine 
  "Clears the global pink.simple engine.  Will clear out active and pending
  functions and evnts from the root audio node, control functions, events."
  []
  (engine-clear engine))

(defn add-afunc 
  "Add an audio function to the root node of the pink.simple engine."
  [afn]
  (engine-add-afunc engine afn))

(defn remove-afunc 
  "Removes an audio function from the root node of the pink.simple engine."
  [afn]
  (engine-remove-afunc engine afn))

(defn add-pre-cfunc 
  "Add a control function to the pre-audio node for the pink.simple engine."
  [cfn]
  (engine-add-pre-cfunc engine cfn))

(defn remove-pre-cfunc 
  "Remove a control function from the pre-audio node for the pink.simple engine."
  [cfn]
  (engine-remove-pre-cfunc engine cfn))

(defn add-post-cfunc
  "Add a control function to the post-audio node for the pink.simple engine."
  [cfn]
  (engine-add-post-cfunc engine cfn))

(defn remove-post-cfunc 
  "Remove a control function from the post-audio node for the pink.simple engine."
  [cfn]
  (engine-remove-post-cfunc engine cfn))

(defn get-tempo
  "Get the current tempo from the engine's built-in event-list."
  []
  (engine-get-tempo engine))

(defn get-tempo-atom
  "Get the current tempo-atom from the engine's built-in event-list."
  []
  (engine-get-tempo-atom engine))

(defn set-tempo 
  "Set the current tempo on the engine's built-in event-list."
  [tempo]
  (engine-set-tempo engine tempo))

(defn add-events 
  "Takes in list of events and adds to engine's event list."
  ([evts]
  (engine-add-events engine evts))
  ([evt & evts]
   (add-events (list* evt evts))))

(defn clear-events
  "Clears all pending events in engine's event list"
  []
  (engine-clear-events engine))

(defn add-audio-events 
  "Takes in list of events, wraps in audio events, and adds to engine's event list."
  ([evts]
  (engine-add-events engine (audio-events engine evts)))
  ([evt & evts]
   (add-audio-events (list* evt evts))))

(defn use-absolute-time-events!
  "Set the Pink simple engine's event list to process events without modifying event
  start times. By default, Pink will process new events' start time as relative to 
  the cur-beat time. When absolute time is set, events will be merged as-is."
  []
  (use-absolute-time! (.event-list ^Engine engine)) )

(defn now 
  "Returns the current *beat* time of the engine."
  ^double []
  (event-list-beat-time (.event-list ^Engine engine)))

(defn tempo 
  "Returns the current *tempo* of the engine."
  ^double []
  (engine-get-tempo ^Engine engine))

;; higher level

(defn apply-afunc-with-dur
  "Applies an afunc to args, wrapping results with (with-duration dur)."
  [afunc dur & args]  
  (with-duration (double dur)
    (apply!*! afunc args)))

(defn i
  "Csound style note events: audio-func, start, dur, & args. 
  Wraps into an event that will call audio-func with args, and wrap 
  with with-duration call with dur. Most likely used in conjunction
  with add-audio-events so that generated afuncs will be added to 
  to an engine."
  [afunc start dur & args]
  (apply event apply-afunc-with-dur start afunc dur args))

(defn with-afunc
  "Wraps note lists with calls to i with audio-func to use."
  ([afunc notelist]
   (map #(apply i afunc %) notelist))
  ([afunc note & notes]
  (with-afunc afunc (list* note notes))))

(defn sco->events 
  "Convert SCO formatted note lists into events by applying i to all notes.
  SCO format follows Csound style note events: audio-func, start, dur, & args."
  [notes]
  (map #(apply i %) notes))


