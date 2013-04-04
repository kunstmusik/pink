(ns audio-seq.envelopes
  "Envelope Generator Functions"
  (:require [audio-seq.engine :refer [*sr*]]
            [audio-seq.util :refer [create-buffer fill swapl!]]))

(defn- make-env-data [pts]
  {:pre (even? (count pts))}
  (let [[x & xs] (partition 2 pts)]
    (second (reduce (fn [[[a b] lst] [c d :as p]] 
              (let [run (double (* c *sr*))
                   rise (double (/ (- d b) run))] 
             [p (conj lst [run rise])] ))
                         [x []] xs))))

(defn- env-get-inc [data counter]
  (loop [cnt 0.0 [x & xs] data]
    (if x
      (let [[a b] x
            c (+ cnt a)]
        (if (< counter c)
          b
          (recur c xs))) 
      0.0)))


(defn env [pts]
 {:pre (even? (count pts))}
  (let [linedata (make-env-data pts)
        cur-val (double-array 1 (nth pts 0))
        counter (long-array 1 -1)
        out (create-buffer)]
  (fn ^doubles[]
    (fill out cur-val #(+ ^double % ^double (env-get-inc linedata (swapl! counter inc)))))))
