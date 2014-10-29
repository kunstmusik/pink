(ns 
  ^{:doc "Simple interface for single-engine projects"
   :author "Steven Yi"}
  pink.simple
  (:require [pink.engine :refer :all]))

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
  [evts]
  (engine-add-events engine evts))

(defn add-audio-events 
  "Takes in list of events, wraps in audio events, and adds to engine's event list."
  [evts]
  (engine-add-events engine (audio-events engine evts)))
