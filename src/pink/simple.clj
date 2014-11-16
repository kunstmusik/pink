(ns 
  ^{:doc "Simple interface for single-engine projects"
   :author "Steven Yi"}
  pink.simple
  (:require [pink.engine :refer :all]
            [pink.util :refer [with-duration]]
            [pink.event :refer [event]]
            ))

(def engine (engine-create :nchnls 2))

(defn start-engine []
  (engine-start engine))

(defn stop-engine []
  (engine-stop engine))

(defn clear-engine []
  (engine-clear engine))

(defn add-afunc [afn]
  (engine-add-afunc engine afn))

(defn remove-afunc [afn]
  (engine-remove-afunc engine afn))

(defn add-pre-cfunc [cfn]
  (engine-add-pre-cfunc engine cfn))

(defn remove-pre-cfunc [cfn]
  (engine-remove-pre-cfunc engine cfn))

(defn add-post-cfunc [cfn]
  (engine-add-post-cfunc engine cfn))

(defn remove-post-cfunc [cfn]
  (engine-remove-post-cfunc engine cfn))

(defn add-events 
  "Takes in list of events and adds to engine's event list."
  ([evts]
  (engine-add-events engine evts))
  ([evt & evts]
   (add-events (list* evt evts))))

(defn add-audio-events 
  "Takes in list of events, wraps in audio events, and adds to engine's event list."
  ([evts]
  (engine-add-events engine (audio-events engine evts)))
  ([evt & evts]
   (add-audio-events (list* evt evts))))


;; higher level

(defn apply-afunc-with-dur
  "Applies an afunc to args, wrapping results with (with-duration dur)."
  [afunc dur & args]  
  (with-duration dur
    (apply afunc args)))

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


