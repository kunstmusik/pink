;; Test of Mutable Control input
;; index is held in an atom, reader reads from atom and returns a buffer

(ns pink.demo.demo4
  (:require [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [env]]
            [pink.oscillators :refer [sine sine2]]
            [pink.util :refer [mul swapd! sum const create-buffer getd setd! arg shared let-s reader]]))


(defn fm-synth [freq]
  (let-s [e (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])] 
    (mul
        (sine2 (sum freq (mul e 440 (sine freq))))
        (mul 0.4 e))))

;; test design work
;; mutable value will be held in an atom
;; reader will be the audio-func to read from the atom

(def index (atom 1))
(def t (reader index))
(reset! index 3.25)

(aget ^doubles (t) 0) 

(defn fm-bell [freq]
  (let-s [e (env [0.0 0.0 0.05 1.0 0.3 0])] 
    (mul
        (sine2 (sum freq (mul freq t (sine (* 4.77 freq)))))
        (mul 0.4 e))))

(defn demo-afunc [e]
  (let [melody (ref (take (* 4 8) (cycle [220 330 440 330])))
        dur 0.25 
        cur-time (double-array 1 0.0)
        time-incr (/ *buffer-size* 44100.0)
        out (create-buffer)]
    (engine-add-afunc e (fm-synth 440))
    (fn ^doubles []
      (let [t (+ (getd cur-time) time-incr)]
        (when (>= t dur)
          (engine-add-afunc e (fm-bell 220)))
        (setd! cur-time (rem t dur)))
      out
      )))




;;

(comment

  (def e (engine-create))
  (engine-start e)
  (engine-add-afunc e (demo-afunc e))
  (engine-stop e)

  (engine-clear e)
  e

  )


