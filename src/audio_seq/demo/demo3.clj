(ns audio-seq.demo.demo2
  (:require [audio-seq.engine :as eng]
            [audio-seq.envelopes :refer [env]]
            [audio-seq.oscillators2 :refer [sine sine2]]
            [audio-seq.util :refer [mix mul const create-buffer getd setd!]]))


(defn fm-synth [freq]
  (fn [] 
    (let [e (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0])] 

      (mul
        (sine2 (mul
                 freq
                 (mul e (sine (* 1 freq)))))
        (mul 0.5 e)))))


(defn demo [e]
  (let [melody (take (* 4 8) (cycle [220 330 440 330]))
        dur 0.25]
    (loop [[x & xs] melody]
      (when x
        (let [afs (e :pending-funcs)]
          (dosync
            (alter afs conj (fm-synth x) (fm-synth (* 2 x))))
          (recur xs))))))


(defn demo-afunc [e]
  (let [melody (ref (take (* 4 8) (cycle [220 330 440 330])))
        dur 0.25 
        cur-time (double-array 1 0.0)
        time-incr (/ eng/*ksmps* 44100.0)
        afs (e :pending-funcs)
        out (create-buffer)]
    (dosync (alter afs conj (fm-synth 440)))
    (fn ^doubles []
      (let [t (+ (getd cur-time) time-incr)]
        (when (> t dur)
          (dosync (alter afs conj (fm-synth 220))))
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


