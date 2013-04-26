(ns audio-seq.demo.demo1
  (:require [audio-seq.engine :as eng]
            [audio-seq.envelopes :refer [env]]
            [audio-seq.oscillators :refer [sine]]
            [audio-seq.util :refer [mix mul const create-buffer getd setd!]])
  (:use [overtone.at-at]))


(defn simple-synth [freq]
  (mul
    (mix 
      (sine freq)
      (mul (const 0.5) (sine (* 2 freq)))
      (mul (const 0.25) (sine (* 3 freq)))
      (mul (const 0.125) (sine (* 4 freq))))
    (mul
      0.25
      (env [0.0 0.0 0.02 1 0.02 0.9 0.2 0.9 0.2 0]))))


(defn demo [e]
  (let [melody (take (* 4 8) (cycle [220 330 440 330]))
        dur 0.25]
    (loop [[x & xs] melody]
      (when x
        (let [afs (e :pending-funcs)]
          (dosync
            (alter afs conj (simple-synth x) (simple-synth (* 2 x))))
        (recur xs))))))


(defn demo-afunc [e]
  (let [melody (ref (take (* 4 8) (cycle [220 330 440 330])))
        dur 0.25 
        cur-time (double-array 1 0.0)
        time-incr (/ eng/*ksmps* 44100.0)
        afs (e :pending-funcs)
        out (create-buffer)]
    (dosync (alter afs conj (simple-synth 440)))
    (fn ^doubles []
      (let [t (+ (getd cur-time) time-incr)]
        (when (> t dur)
          (dosync (alter afs conj (simple-synth 440))))
        (setd! cur-time (rem t dur)))
      out
      )))


;;

(comment

  (defn note-sender[e]
    (dosync
     (alter (e :pending-funcs) conj (simple-synth 440) (simple-synth 660))))

(def my-pool (mk-pool))

(def e (eng/engine-create))
(eng/engine-start e)
(dosync (alter (e :pending-funcs) conj (demo-afunc e)))
(eng/engine-stop e)
(eng/engine-clear e)
(every 250 (partial note-sender e) my-pool :fixed-delay true)
(every 250 (partial note-sender e) my-pool)
         
(stop-and-reset-pool! my-pool)
e


  (let [e (eng/engine-create)]
    (eng/engine-start e)
    (demo e)
    (Thread/sleep 500)
    (eng/engine-stop e))

  (let [e (eng/engine-create)]
    (eng/engine-start e)
    (demo e)
    (Thread/sleep 500)
    (eng/engine-stop e)))


