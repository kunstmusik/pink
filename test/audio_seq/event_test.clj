(ns audio-seq.event-test
  (:require [audio-seq.event :refer :all]
            [clojure.test :refer :all]
            )
  )

(defn test-audio-func [])

(deftest event-test
  (let [evt (event test-audio-func 0.5 1.0 4.0 :test)]
   (is (= 0.5 (:start evt))) 
   (is (= 1.0 (:duration evt))) 
   (is (= [4.0 :test] (:init-args evt))) 
   (is (= test-audio-func (:init-func evt))) 
    ))

(deftest event-list-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        evtlst (event-list [test-note])]
    (is (= #{} @(.active evtlst)))
    (is (= #{test-note} @(.inactive evtlst)))
    (is (= #{test-note} @(.evts evtlst))) 
    ))

(deftest event-list-add-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        test-note-dupe (event test-audio-func 0.0 1.0 440.0)
        test-note2 (event test-audio-func 0.0 1.1 880.0)
        test-note3 (event test-audio-func 0.0 1.0 220.0)
        evtlst (event-list [test-note2])]
    (event-list-add evtlst test-note)
    (is (= #{} @(.active evtlst)))
    (is (= #{test-note test-note2} @(.inactive evtlst)))
    (is (= #{test-note test-note2} @(.evts evtlst))) 

    ;;Test that adding same note is skipped
    (event-list-add evtlst test-note)
    (is (= 2 (count @(.evts evtlst))))

    (event-list-add evtlst test-note3)
    (is (= 3 (count @(.evts evtlst))))
    (is (= #{test-note test-note2 test-note3} @(.evts evtlst))) 


    ;;Test that adding notes with same values is allowed 

    (event-list-add evtlst test-note-dupe)
    (is (= 4 (count @(.evts evtlst))))
    (is (= #{test-note test-note2 test-note3 test-note-dupe} @(.evts evtlst))) 
    ))

(deftest event-list-remove-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        test-note2 (event test-audio-func 0.0 1.0 880.0)
        evtlst (event-list [test-note]) ]
    (event-list-add evtlst test-note2)
;    (print "\n OMG " @(.evts evtlst) "\n")
    (event-list-remove evtlst test-note)
    (is (= #{} @(.active evtlst)))
    (is (= #{test-note2} @(.inactive evtlst)))
    (is (= #{test-note2} @(.evts evtlst))) 
    ))

