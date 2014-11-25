(ns 
  ^{:doc "Dynamically-scoped variables used to alter the context of processing."
   :author "Steven Yi"}
  pink.config)

(def ^:dynamic *sr* 44100)
(def ^:dynamic *buffer-size* 64)
(def ^:dynamic *nchnls* 1)
(def ^:dynamic *current-buffer-num* 0)
(def ^:dynamic *duration* nil)
(def ^:dynamic *done* nil)
