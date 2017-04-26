(ns 
  ^{:doc "Dynamically-scoped variables used to represent the context of
         processing."
   :author "Steven Yi"}
  pink.config)

(def
  ^{:dynamic true
    :doc "The current processing engine."}
  *engine* nil)

(def 
  ^{:dynamic true
    :doc "Sample-rate of processing context/engine." } 
  *sr* 44100)

(def 
  ^{:dynamic true
    :doc "Size of buffer for engine (i.e., number of samples to
         generate/process per buffer)." } 
  *buffer-size* 64)

(def 
  ^{:dynamic true
    :doc "Number of channels configured for processing context/engine." } 
  *nchnls* 1)

(def 
  ^{:dynamic true
    :doc "The number of buffers that have processed since engine start.
         Multiply by *buffer-size* to get time in samples, then divide time in
         samples by *sr* to get time in seconds." } 
 *current-buffer-num* 0)

(def 
  ^{:dynamic true 
    :doc "Time in beats for duration. This context variable may not be set, and
         not all audio functions may use this value." } 
 *duration* nil)

(def 
  ^{:dynamic true 
    :doc "When used, *done* will hold a 1-element boolean array that holds a
         boolean to signal done-ness.  Useful in contexts where a signal graph
         (i.e., instrument) will play until a signal is given, for example:
        
         1. MIDI note on => start instrument
         2. MIDI note off => mark flag done, instrument gracefully turns off

         Envelope generators will be the code most likely to be designed to
         look for and use *done*." } 
 *done* nil)

(def 
  ^{:dynamic true 
    :doc "Tempo of the current processing context. Useful to calculate things
         like delay times that are synced with the tempo." } 
 *tempo* 60.0)

(def 
  ^{:dynamic true 
    :doc "Current time in beats. A continuous value that may change at
         different rates depending upon tempo changes. Useful for scheduling
         events relative to beat time." } 
 *beat* 0.0)

