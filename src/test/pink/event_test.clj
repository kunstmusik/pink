(ns pink.event-test
  (:require [pink.event :refer :all]
            [clojure.test :refer :all]
            [pink.config :refer :all])
  (:import [java.util PriorityQueue]
           [pink.event Event EventList])
  )

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(defn test-audio-func [])

(deftest event-test
  (let [evt ^Event (event test-audio-func 0.5 1.0 4.0 :test)]
   (is (= 0.5 (.start evt))) 
   (is (= [1.0 4.0 :test] (.event-args evt))) 
   (is (= test-audio-func (.event-func evt))) )
  ;test no-arg event
  (let [evt ^Event (event test-audio-func 0.5)]
   (is (= 0.5 (.start evt))) 
   (is (= [] (.event-args evt))) 
   (is (= test-audio-func (.event-func evt))) 
    ))

(deftest event-list-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        evtlst ^EventList (event-list [test-note] *buffer-size* *sr*)
        events ^PriorityQueue (.events evtlst)]
    (is (= 1 (.size events))) 
    (is (= test-note (.peek events)))
    
    ))

(deftest alter-event-time-test
  (let [evt ^Event (event test-audio-func 0.5)
        evt2 ^Event (alter-event-time 0.0 evt)
        ]
   (is (= 0.0 (.start evt2))) 
   (is (= [] (.event-args evt2))) 
   (is (= test-audio-func (.event-func evt2)))
   
   (is (= (.event-args evt) (.event-args evt2))) 
   (is (= (.event-func evt) (.event-func evt2)))
   ))

(defn event-equals
  [^Event e1 ^Event e2]
  (and (= (.event-func e1) (.event-func e2))
       (= (.start e1) (.start e2))
       (= (.event-args e1) (.event-args e2))))

(deftest event-list-add-test
  (with-private-fns [pink.event [merge-pending!]] 
    (let [test-note (event test-audio-func 0.0 1.0 440.0)
        test-note-dupe (event test-audio-func 0.0 1.0 440.0)
        test-note2 (event test-audio-func 0.1 1.1 880.0)
        test-note3 (event test-audio-func 0.2 1.0 220.0)
        evtlst (event-list [test-note2] *buffer-size* *sr*)
        events ^PriorityQueue (.events evtlst)]

    (event-list-add evtlst test-note3)
    (event-list-add evtlst test-note)
    (event-list-add evtlst test-note-dupe)

    (merge-pending! evtlst)

    (is (= 4 (.size events)))
    (is (event-equals test-note (.poll events)))
    (is (event-equals test-note-dupe (.poll events)))
    (is (event-equals test-note2 (.poll events)))
    (is (event-equals test-note3 (.poll events)))

    ;;;Test that adding same note is skipped
    ;(event-list-add evtlst test-note)
    ;(is (= 2 (count (:events evtlst))))


    )))

;(deftest event-list-remove-test
;  (let [test-note (event test-audio-func 0.0 1.0 440.0)
;        test-note2 (event test-audio-func 0.0 1.0 880.0)
;        evtlst (event-list [test-note]) ]
;    (event-list-add evtlst test-note2)
;;    (print "\n OMG " @(:events evtlst) "\n")
;    (event-list-remove evtlst test-note)
;    (is (= [test-note2] @(:events evtlst))) 
;    ))

