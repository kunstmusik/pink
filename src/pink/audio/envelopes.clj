(ns pink.audio.envelopes
  "Envelope Generator Functions"
  (:require [pink.audio.engine :refer [*sr*]]
            [pink.audio.util :refer [create-buffer fill swapl! getl]]))

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

(defn env
  "Generates an envelop given time-tagged pairs of values (t0, v0, t1, v1 ...)"
 [pts] 
 {:pre (even? (count pts))}
  (let [linedata (make-env-data pts)
        line-samples (reduce + (map first linedata))
        cur-val (double-array 1 (nth pts 1))
        counter (long-array 1 -1)
        out (create-buffer)]
 ;; (clojure.pprint/pprint linedata)
  ;;(println "Samples: " line-samples)
  (fn ^doubles[]
;;    (println "Slope: " (env-get-inc linedata (getl counter)))
    (when (<= (getl counter) line-samples)
      (fill out cur-val 
        #(if-let [x ^double (env-get-inc linedata (swapl! counter inc))]
          (+ ^double % x)
          0.0))))))


;; EXPONENTIAL ENVELOPE


(defn- make-exp-env-data [pts]
  {:pre (even? (count pts))}
  (let [[x & xs] (partition 2 pts)]
    (second (reduce (fn [[[a b] lst] [c d :as p]] 
                      (let [cnt (double (* c *sr*))
                            mlt (double (Math/pow (/ d b) (/ 1.0 cnt)))] 
                        [p (conj lst [b mlt cnt])] ))
                    [x []] xs))))



(defn- exp-env-get-inc [data counter]
  (loop [cnt 0.0 [x & xs] data]
    (when x
      (let [[value mlt dur] x
            c (+ cnt dur)]
        (if (< counter c)
          mlt
          (recur c xs))))))

(defn exp-env [pts]
 {:pre (even? (count pts))}
  (let [linedata (make-exp-env-data pts)
        line-samples (reduce + (map #(nth % 2) linedata))
        cur-val (double-array 1 (nth pts 1))
        counter (long-array 1 -1)
        out (create-buffer)]
 ;; (clojure.pprint/pprint linedata)
  ;;(println "Samples: " line-samples)
  (fn ^doubles[]
;;    (println "Slope: " (env-get-inc linedata (getl counter)))
    (when (<= (getl counter) line-samples)
      (fill out cur-val 
        #(if-let [x (exp-env-get-inc linedata (swapl! counter inc))]
          (* ^double % x)
          0.0))))))


;; Simple Envs

(defn adsr 
  "Linear Attack-Decay-Sustain-Release Envelope"
  [a d s r & {:keys [dur] :or {dur 1.0}}]
  (env [0.0 0.0 a 1.0 d s dur s r 0.0]))


(defn xadsr 
  "Exponential Attack-Decay-Sustain-Release Envelope"
  [a d s r & {:keys [dur] :or {dur 1.0}}]
  (exp-env [0.0 0.00001 a 1.0 d s dur s r 0.00001]))


(defn xar 
  "Exponential Attack-Release Envelope"
  [a r]
  (exp-env [0.0 0.00001 a 1.0 r 0.00001]))

(comment

  (def pts [0.0 0.001 0.05 1.0 0.3 0.001])
  (def t (exp-env pts))
  (def pts-data (make-exp-env-data pts))
  (def pts-data2 (make-env-data pts))
  (map print pts-data)
  (map print pts-data2)
  (t)

  )
