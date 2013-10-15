(ns audio-seq.audio.engine-test
  (:use [audio-seq.audio.engine])
  (:use clojure.test))

(deftest test-engine-kill-all
  (engines-clear)
  (let [a (engine-create) 
        b (engine-create)]
    (is (= :stopped (engine-status a)))
    (is (= :stopped (engine-status b)))
    (is (not @(a :clear)))
    (is (not @(b :clear)))

    (is a)
    (is b)

    (dosync
      (ref-set (a :status) :running)
      (ref-set (b :status) :running))

    (is (= :running (engine-status a)))
    (is (= :running (engine-status b)))

    (engine-kill-all)  

    (is (true? @(a :clear)))
    (is (true? @(b :clear)))


    ))
