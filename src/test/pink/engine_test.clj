(ns pink.engine-test
  (:require [pink.engine :refer :all]
            [pink.event :refer :all]
            [clojure.test :refer :all])
  (:import [pink.engine Engine]))

;; Hmm, I don't remember what this tests, if anything...
;; should definitely rewrite this one! :)
(deftest test-engine-kill-all
  (engines-clear)
  (let [a ^Engine (engine-create)
        b ^Engine (engine-create)]
    (is (= :stopped (engine-status a)))
    (is (= :stopped (engine-status b)))
    (is (not @(.clear a)))
    (is (not @(.clear b)))

    (is a)
    (is b)

    (dosync
      (reset! (.status a) :running)
      (reset! (.status b) :running))

    (is (= :running (engine-status a)))
    (is (= :running (engine-status b)))

    (engine-kill-all)  

    (is (true? @(.clear a)))
    (is (true? @(.clear b)))


    ))

(deftest audio-events-test
  (let [test-func #() 
        e (engine-create) 
        evts (audio-events 
               e
               (map #(apply event %) 
                    [[test-func 0.5]
                     [test-func 0.0]]))]
      (is (= 2 (count evts)))
      ))
