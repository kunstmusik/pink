(ns audio-seq.event
  (:require [audio-seq.util :as util]
            [audio-seq.protocols :refer :all]))

(defn event [f start end & args]
  {:init-func f 
   :perf-func nil 
   :init-args args
   :start start
   :duration end
   }
  )

(defn events [f & args]
  (map #(apply event f %) args))

;(defn- process-events)

;(defn event-block [evts]
;  (let)
;  (reify AudioFunc 
;    (process [this] )
;    ))


(comment

  (defn test-func [])
  (events test-func [1 2 3] [4 5 6])

  )
