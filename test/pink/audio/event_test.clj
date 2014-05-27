(ns pink.audio.event-test
  (:require [pink.event :refer :all]
            [clojure.test :refer :all]
            )
  )

(defn test-audio-func [])

(deftest event-test
  (let [evt (event test-audio-func 0.5 1.0 4.0 :test)]
   (is (= 0.5 (.start evt))) 
   (is (= [1.0 4.0 :test] (.event-args evt))) 
   (is (= test-audio-func (.event-func evt))) 
    ))

(deftest event-list-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        evtlst (event-list [test-note])]
    (is (= [test-note] @(:events evtlst))) 
    ))

(deftest event-list-add-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        test-note-dupe (event test-audio-func 0.0 1.0 440.0)
        test-note2 (event test-audio-func 0.1 1.1 880.0)
        test-note3 (event test-audio-func 0.2 1.0 220.0)
        evtlst (event-list [test-note2])]
    (event-list-add evtlst test-note)
    (is (= [test-note test-note2] @(:events evtlst))) 

    ;;Test that adding same note is skipped
    (event-list-add evtlst test-note)
    (is (= 2 (count @(:events evtlst))))

    (event-list-add evtlst test-note3)
    (is (= 3 (count @(:events evtlst))))
    (is (= [test-note test-note2 test-note3] @(:events evtlst))) 


    ;;Test that adding notes with same values is allowed 

    (event-list-add evtlst test-note-dupe)
    (is (= 4 (count @(:events evtlst))))
    (is (= [test-note-dupe test-note  test-note2 test-note3 ] @(:events evtlst))) 
    ))

(deftest event-list-remove-test
  (let [test-note (event test-audio-func 0.0 1.0 440.0)
        test-note2 (event test-audio-func 0.0 1.0 880.0)
        evtlst (event-list [test-note]) ]
    (event-list-add evtlst test-note2)
;    (print "\n OMG " @(:events evtlst) "\n")
    (event-list-remove evtlst test-note)
    (is (= [test-note2] @(:events evtlst))) 
    ))

