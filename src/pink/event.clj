(ns pink.event
  (:require 
            [pink.audio.protocols :refer :all]
            ))

(deftype Event [event-func event-args ^Double start]
 ; Object
 ;(toString [e]  (format "\t%s\t%s\t%s\n" event-func start event-args )) 
  )

;;(deftype Event [init-func perf-func init-args start duration state]
;;;  AudioFunc
;;;  (process [this] nil)
;;  Object
;;  (toString [this]
;;      
;;    )
;;  )

(defn event [f start & args]
  ;{:event-func f 
  ; :event-args args
  ; :start start
  ; }
  (Event. f args start)
  )


(defn events [f & args]
  (map #(apply event f %) args))

;(defn- process-events)

;(defn event-block [evts]
;  (let)
;  (reify AudioFunc 
;    (process [this] )
;    ))

;; NOTE - wondering if this should be something that implements ISeq or Set interface, as an event list is something like a sorted list of events, with active and inactive being separate views...
;(deftype EventList [evts active inactive curtime]
;  AudioFunc
;  (process [this] nil)
;  )

(defn event-list
  "Creates an EventList. 
  
  EventList's its own internal time and fires off events whose start times have
  been met.  Event have no notion of duration. An event may do things like 
  schedule an audio function to be added to an engine's performance list, force
  turning off an audio function, and so on."

  ([] (event-list []))
  ([evts] 
   {:events (ref (sort-by #(.start ^Event %) evts))       
    :curevent nil
    :cur-buffer 0
    }))

(defn event-list-add [evtlst ^Event evt]
  "Add an event to an event list"
  (do (when (< (.indexOf @(:events evtlst) evt) 0)
    (dosync
        (alter (:events evtlst) (fn [a] (sort-by #(.start ^Event %) (conj a evt))))))
  evtlst))

(defn event-list-remove [evtlst evt]
  "remove an event from the event list"
  (do
    (dosync
      (alter (:events evtlst) (fn [a] (remove #(= % evt) a)))) 
    evtlst))

(defn event-list-tick [evtlst] 
       
  )

(comment


  (defn test-func [])
  (events test-func [1 2 3] [4 5 6])

(def test-note (event test-func 0.0 1.0 440.0))
(def test-note-dupe (event test-func 0.0 1.0 440.0))
(def test-note2 (event test-func 0.0 1.0 220.0))
(def test-note3 (event test-func 1.0 1.5 110.0))
(print (.start test-note3))
(def evtlst (event-list [test-note]))
(event-list-add evtlst test-note2)

(event-list-remove evtlst test-note)
(print evtlst)

  )
