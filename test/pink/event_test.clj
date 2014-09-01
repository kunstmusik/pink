(ns pink.event-test
  (:require [pink.event :refer :all]
            [clojure.test :refer :all])
  (:import [java.util PriorityQueue])
  )

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(defn test-audio-func [])

(deftest event-test
  (let [evt ^pink.event.Event (event test-audio-func 0.5 1.0 4.0 :test)]
   (is (= 0.5 (.start evt))) 
   (is (= [1.0 4.0 :test] (.event-args evt))) 
   (is (= test-audio-func (.event-func evt))) 
    ))

(deftest event-list-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        evtlst (event-list [test-note])
        events ^PriorityQueue (.events evtlst)]
    (is (= 1 (.size events))) 
    (is (= test-note (.peek events)))
    
    ))

(deftest event-list-add-test
  (with-private-fns [pink.event [merge-pending!]] 
    (let [test-note (event test-audio-func 0.0 1.0 440.0)
        test-note-dupe (event test-audio-func 0.0 1.0 440.0)
        test-note2 (event test-audio-func 0.1 1.1 880.0)
        test-note3 (event test-audio-func 0.2 1.0 220.0)
        evtlst (event-list [test-note2])
        events ^PriorityQueue (.events evtlst)]
    (event-list-add evtlst test-note)
    (merge-pending! evtlst)
    (is (= [test-note test-note2] (into [] (.toArray events)))) 

    ;;;Test that adding same note is skipped
    ;(event-list-add evtlst test-note)
    ;(is (= 2 (count (:events evtlst))))

    (event-list-add evtlst test-note3)
    (merge-pending! evtlst)
    (is (= 3 (.size events)))
    (is (= [test-note test-note2 test-note3] (into [] (.toArray events)))) 


    ;;Test that adding notes with same values is allowed 
    (event-list-add evtlst test-note-dupe)
    (merge-pending! evtlst)
    (is (= 4 (.size events)))
    (is (= [test-note test-note-dupe test-note2 test-note3] (into [] (.toArray events)))) 
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

