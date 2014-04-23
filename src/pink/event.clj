(ns pink.event
  (:require 
            [pink.audio.protocols :refer :all]
            ))

;;(deftype Event [init-func perf-func init-args start duration state]
;;;  AudioFunc
;;;  (process [this] nil)
;;  Object
;;  (toString [this]
;;      
;;    )
;;  )

(defn event [f start end & args]
  {:init-func f 
   :perf-func nil 
   :init-args args
   :start start
   :duration end
   :state (atom :inactive)
   }
  )

(defn event-comparator [a b]
  "comparator function for sorted set of events"
  (if (= a b)
    0
    (let [x (compare (:start a) (:start b))]
      (if (not= 0 x)
        x 
        (let [y (compare (:duration a) (:duration b))]
          (if (not= 0 y)
            y
            -1))))))


(defn events [f & args]
  (map #(apply event f %) args))

;(defn- process-events)

;(defn event-block [evts]
;  (let)
;  (reify AudioFunc 
;    (process [this] )
;    ))

;; NOTE - wondering if this should be something that implements ISeq or Set interface, as an event list is something like a sorted list of events, with active and inactive being separate views...
(deftype EventList [evts active inactive curtime]
  AudioFunc
  (process [this] nil)
  )

(defn event-list
  "Creates an EventList"
  ([] (event-list []))
  ([evts] 
   (EventList.
     (ref (apply sorted-set-by event-comparator evts)) 
     (ref #{})
     (ref (set evts)) 
     0.0)))

(defn event-list-add [^EventList evtlst evt]
  "Add and event to an event list"
  (do
    (dosync
      (alter (.evts evtlst) conj evt)
      (alter (.inactive evtlst) conj evt))
    evtlst))

(defn event-list-remove [^EventList evtlst evt]
  "remove an event from the event list"
  (do
    (dosync
      (alter (.evts evtlst) disj evt)
      (alter (.active evtlst) disj evt)
      (alter (.inactive evtlst) disj evt)) 
    evtlst))

(comment


  (defn test-func [])
  (events test-func [1 2 3] [4 5 6])

(def test-note (event test-func 0.0 1.0 440.0))
(def test-note-dupe (event test-func 0.0 1.0 440.0))
(def test-note2 (event test-func 0.0 1.0 220.0))
(def test-note3 (event test-func 1.0 1.5 110.0))

(def n (apply sorted-set-by event-comparator [test-note test-note2 test-note3]))
(= n #{test-note test-note2 test-note3})  

(def evtlst (event-list [test-note]))
(event-list-add evtlst test-note2)
(event-list-remove evtlst test-note)
  )
