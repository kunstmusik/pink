(ns pink.control
  ^{:doc "Library for useful control functions."
   :author "Steven Yi"}
  (:require [pink.config :refer :all])
  )

;; Control Functions
(defn create-clock
  "Creates a sample-accurate clock control function that triggers a trigger-fn 
  according to the tempo held within the tempo-atom atom.  When the time has been 
  met, it will call the given trigger-fn and truncate the running sample-count.
  
  User can supply an optional state-atom for signaling to the clock for
  different running states. Acceptable states are :running, :paused, and :done. 
  Any other state will result in :done.
  
  User may also supply an optional done-fn. done-fn will be called when this 
  clock goes into the :done state. done-fn must be a 0-arity function."
  ([tempo-atom trigger-fn]
   (create-clock tempo-atom trigger-fn (atom :running)))
  ([tempo-atom trigger-fn state-atom]
   (create-clock tempo-atom trigger-fn state-atom nil))
  ([tempo-atom trigger-fn state-atom done-fn]
   (let [sr (double *sr*)
         buffer-size (long *buffer-size*)
         init-val (long (* sr (/ 60.0 (double @tempo-atom)))) 
         ^longs sample-count (long-array 1 init-val)]
     (fn []
       (condp = @state-atom
        :running
        (let [num-samples-to-wait (long (* sr (/ 60.0 (double @tempo-atom))))
              cur-samp (aget sample-count 0)]
          (if (>= cur-samp num-samples-to-wait)
            (do 
              (aset sample-count 0 (rem cur-samp num-samples-to-wait))
              (trigger-fn))
            (aset sample-count 0 (+ cur-samp buffer-size)))
          true)
        :paused 
          true ;; don't advance clock in any way, just return true
        (do 
          (when done-fn (done-fn)) 
          false)
         )))))

