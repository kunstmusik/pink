;; Test of Exponential Envelope 
;; index is held in an atom, reader reads from atom and returns a buffer

(ns pink.demo.demo5
  (:require [pink.audio.engine :as eng]
            [pink.audio.envelopes :refer [env exp-env adsr xadsr xar]]
            [pink.audio.oscillators :refer [sine sine2]]
            [pink.audio.util :refer [mix mul swapd! sum const create-buffer getd setd! arg shared let-s reader]]))


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

(aget (t) 0) 

(defn fm-bell [freq]
  (
   ;let-s [e (exp-env [0.0 0.00001 0.05 1.0 3 0.000001])] 
   let-s [e (xar 0.0001 1.3)] 
    (mul
        (sine2 (sum freq (mul freq t (sine (* 4.77 freq)))))
        (mul 0.2 e))))

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

  (eng/engines-clear)

  e

  (let [e (eng/engine-create)]
    (eng/engine-start e)
    (demo e)
    (Thread/sleep 500)
    (eng/engine-stop e)))


