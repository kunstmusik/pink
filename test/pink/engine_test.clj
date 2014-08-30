(ns pink.engine-test
  (:require [pink.engine :refer :all])
  (:require [clojure.test :refer :all]))

(deftest test-engine-kill-all
  (engines-clear)
  (let [a ^pink.engine.Engine (engine-create) 
        b ^pink.engine.Engine (engine-create)]
    (is (= :stopped (engine-status a)))
    (is (= :stopped (engine-status b)))
    (is (not @(.clear a)))
    (is (not @(.clear b)))

    (is a)
    (is b)

    (dosync
      (ref-set (.status a) :running)
      (ref-set (.status b) :running))

    (is (= :running (engine-status a)))
    (is (= :running (engine-status b)))

    (engine-kill-all)  

    (is (true? @(.clear a)))
    (is (true? @(.clear b)))


    ))
