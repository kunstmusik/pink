(ns audio-seq.envelopes
  "Envelope Generator Functions"
  (:require [audio-seq.engine :refer [*sr*]]
            [audio-seq.util :refer [create-buffer fill swapl! getl]]))

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
    (when x
      (let [[a b] x
            c (+ cnt a)]
        (if (< counter c)
          b
          (recur c xs))))))

;;(defn- env-complete? [counter linedata] 
;;  (> counter (first (last (linedata)))))

;;(defn- not-env-complete

(defn env [pts]
 {:pre (even? (count pts))}
  (let [linedata (make-env-data pts)
        line-samples (reduce + (map first linedata))
        cur-val (double-array 1 (nth pts 0))
        counter (long-array 1 -1)
        out (create-buffer)]
 ;; (clojure.pprint/pprint linedata)
  ;;(println "Samples: " line-samples)
  (fn ^doubles[]
;;    (println "Slope: " (env-get-inc linedata (getl counter)))
    (when (<= (getl counter) line-samples)
      (fill out cur-val 
        #(if-let [x (env-get-inc linedata (swapl! counter inc))]
          (+ ^double % x)
          0.0))))))
