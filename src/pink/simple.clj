(ns 
  ^{:doc "Simple interface for single-engine projects"
   :author "Steven Yi"}
  pink.simple
  (:require [pink.engine :refer :all]))

(def engine (engine-create))

(defn start-engine []
  (engine-start engine))

(defn stop-engine []
  (engine-stop engine))

(defn clear-engine []
  (engine-clear engine))

(defn add-afunc [afn]
  (engine-add-afunc engine afn))

(defn remove-afunc [afn]
  (engine-remove-afunc afn))

(defn add-pre-cfunc [cfn]
  (engine-add-pre-cfunc engine cfn))

(defn remove-pre-cfunc [cfn]
  (engine-remove-pre-cfunc engine cfn))

(defn add-post-cfunc [cfn]
  (engine-add-post-cfunc engine cfn))

(defn remove-post-cfunc [cfn]
  (engine-remove-post-cfunc engine cfn))

(defn add-events [evts]
  (engine-add-events engine evts))
