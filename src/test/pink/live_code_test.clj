(ns pink.live-code-test
  (:require [pink.live-code :refer :all]
            [clojure.test :refer :all]))


(deftest test-redef!
  (testing "redef! redefines a to b"
    (defn a [] 1)
    (defn b [] 2)
  

    (is (= 1 (a)))
    (is (= 2 (b)))

    (redef! a b)

    (is (= 2 (a)))
    (is (= 2 (b)))

    (defn b [] 3)

    (is (= 2 (a)))
    (is (= 3 (b)))
    ))


(deftest test-kill-recur!
  (testing "kill-recur! redefines function to n-arity"
    (defn a [x y] (+ x y))
    (is (= 5 (a 2 3)))
    (kill-recur! a)
    (is (nil? (a 2 3)))
    ))

(deftest test-next-beat
  (is (= (- 84 81.11) (next-beat 81.11 4)))
  (is (= (- 82 81.11) (next-beat 81.11 2)))
  (is (= (- 84 81.98) (next-beat 81.98 2)))
  (is (= (- 81.5 81.11) (next-beat 81.11 1/2)))
  (is (= (- 1032.5 1031.998) (next-beat 1031.998 1/2)))
  (is (= (- 4.0 2.25343 )) (next-beat 2.25343 7/4)))

