(ns pink.engine-test
  (:import [pink.engine Engine])
  (:require [pink.engine :refer :all])
  (:require [clojure.test :refer :all]))

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
