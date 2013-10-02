;; Test of FM synthesis

(ns audio-seq.demo.demo4
  (:require [audio-seq.engine :as eng]
            [audio-seq.envelopes :refer [env]]
            [audio-seq.oscillators2 :refer [sine sine2]]
            [audio-seq.util :refer [mix mul swapd! sum const create-buffer getd setd! arg shared let-s]]))


(defn fm-synth [freq]
  (let-s [e (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])] 
    (mul
        (sine2 (sum freq (mul e 440 (sine freq))))
        (mul 0.4 e))))

;; test design work
;; mutable value will be held in an atom
;; reader will be the audio-func to read from the atom

(def index (atom 1))
(defn reader [atm] 
  (let [last (atom 0)
        buffer (atom (create-buffer))
        ]
    (when (not= @atm @last)
      (swapd! buffer (fn [a] @atm))
      (reset last @atm))
    buffer))

(reader index)

(defn fm-bell [freq]
  (let-s [e (env [0.0 0.0 0.05 1.0 0.3 0])] 
    (mul
        (sine2 (sum freq (mul 880.0 (sine (* 4.77 freq)))))
        (mul 0.4 e))))

(defn demo-afunc [e]
  (let [melody (ref (take (* 4 8) (cycle [220 330 440 330])))
        dur 0.4
        cur-time (double-array 1 0.0)
        time-incr (/ eng/*ksmps* 44100.0)
        afs (e :pending-funcs)
        out (create-buffer)]
    (dosync (alter afs conj (fm-synth 440)))
    (fn ^doubles []
      (let [t (+ (getd cur-time) time-incr)]
        (when (> t dur)
          (dosync (alter afs conj (fm-bell 220))))
        (setd! cur-time (rem t dur)))
      out
      )))


;;

(comment

  (defn note-sender[e]
    (dosync
      (alter (e :pending-funcs) conj (fm-synth 440) (fm-synth 660))))

  (def e (eng/engine-create))
  (eng/engine-start e)
  (eng/engine-add-afunc e (demo-afunc e))
  (eng/engine-stop e)

  (eng/engine-clear e)
  e

  (let [e (eng/engine-create)]
    (eng/engine-start e)
    (demo e)
    (Thread/sleep 500)
    (eng/engine-stop e)))


