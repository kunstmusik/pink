(ns pink.demo.demo1
  (:require [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [env]]
            [pink.oscillators :refer [sine]]
            [pink.util :refer [sum mul const create-buffer getd setd!]]))


(defn simple-synth [freq]
  (mul
    (sum 
      (sine freq)
      (mul 0.5 (sine (* 2 freq)))
      (mul 0.25 (sine (* 3 freq)))
      (mul 0.125 (sine (* 4 freq))))
    (mul
      0.25
      (env [0.0 0.0 0.02 1 0.02 0.9 0.2 0.9 0.2 0]))))


(defn demo [e]
  (let [melody (take (* 4 8) (cycle [220 330 440 330]))
        dur 0.25]
    (loop [[x & xs] melody]
      (when x
        (engine-add-afunc e (simple-synth x))
        (engine-add-afunc e (simple-synth (* 2 x)))
        (recur xs)))))


(defn demo-afunc [e]
  (let [melody (ref (take (* 4 8) (cycle [220 330 440 330])))
        dur 0.25 
        cur-time (double-array 1 0.0)
        time-incr (/ *buffer-size* 44100.0)
        out (create-buffer)]
    (engine-add-afunc e (simple-synth 440))
    (fn ^doubles []
      (let [t (+ (getd cur-time) time-incr)]
        (when (> t dur)
          (engine-add-afunc e (simple-synth 440)))
        (setd! cur-time (rem t dur)))
      out
      )))


;;

(comment

  ;(defn note-sender[e]
  ;  (dosync
  ;   (alter (e :pending-funcs) conj (simple-synth 440) (simple-synth 660))))

(def e (engine-create))
(engine-start e)
(engine-add-afunc e (demo-afunc e))
(engine-stop e)
(engine-clear e)

e

  (let [e (engine-create)]
    (engine-start e)
    (demo e)
    (Thread/sleep 500)
    (engine-stop e))

  )


