(ns pink.audio.envelopes
  "Envelope Generator Functions"
  (:require 
    [clojure.pprint :refer [pprint]]
    [pink.audio.engine :refer [*sr*]]
            [pink.audio.util :refer [create-buffer fill swapl! getl setl! getd setd!]]))

(defn- make-env-data 
  "Takes in a list of time tagged pairs (time value) and function for
  calculating the cached value and then creates a list with initial value,
  followed by a list of pairs of (num-samples, cached-value). opfn should
  take in [end start run]"
  [pts opfn]
  {:pre (even? (count pts))}
  (let [pairs (partition 2 pts)
        [x & xs] (if (not= (double (ffirst pairs)) 0.0)
                   (cons [0.0 (second (first pairs))] pairs)
                   pairs)]
     
    (cons (second x) (second (reduce 
              (fn [[[a b] lst] [c d :as p]] 
                (let [run (double (* c *sr*))
                      rise (double (opfn d b run))
                      last-pt (last lst) 
                      total-run (if (nil? last-pt) 0.0 (first last-pt))] 
                      [p (conj lst [(+ total-run run) rise])] ))
                         [x []] xs)))))

(defn get-line-pt 
  [sample linedata]
  (loop [[x & xs] linedata]
    (when x
      (if (< sample (first x))
        x
        (recur xs)))))

(defn env
  "Generates an envelope given pairs of values (t0, v0, t1, v1 ...) where tx is duration of segment."
  [pts] 
  {:pre (even? (count pts))}
  (let [[start & linedata] (make-env-data pts #(/ (- %1 %2) %3))
        cur-val (double-array 1 start)
        counter (long-array 1 0)
        ^doubles out (create-buffer)
        len (alength out)]
    (fn ^doubles[]
      (let [cnt (getl counter)
            [last-sam increment] (get-line-pt cnt linedata)]
        (if (and last-sam increment) 
          (loop [end last-sam
                 incr increment 
                 v (getd cur-val)
                 c cnt
                 i 0]
            (if (< i len)
              (if (and end incr)
                (if (< c end)
                  (let [new-v (+ v incr)]
                    (aset out i new-v)
                    (recur end incr new-v (unchecked-inc c) (unchecked-inc-int i)))
                  (let [[new-end new-incr] (get-line-pt c linedata)]
                    (recur new-end new-incr v c i)))
                (do 
                  (aset out i 0.0)
                  (recur nil nil 0.0 (unchecked-inc c) (unchecked-inc-int i))))
              (do
                (aset cur-val 0 v)
                (aset counter 0 c)
                out)))
          nil)))))


;; EXPONENTIAL ENVELOPE

(defn- adjust-for-zero
  "for exponential envelopes, can not have zero"
  [x]
  (if (zero? x)
    0.0000000001
    x))

(defn- make-exp-env-data [pts]
  {:pre (even? (count pts))}
  (let [adjusted-pts 
        (map #(if (even? %2) %1 (adjust-for-zero %1)) pts (range))]
   (make-env-data adjusted-pts #(Math/pow (/ %1 %2) (/ 1.0 %3)))))


;; TODO - this is almost exactly the same as env except
;; the calculation for new-v.  Should probably make a 
;; macro of the body and share between env and exp-env
(defn exp-env
  "Generates an exponential envelope given pairs of values (t0, v0, t1, v1 ...) where tx is duration of segment."
  [pts] 
  {:pre (even? (count pts))}
  (let [[start & linedata] (make-exp-env-data pts)
        cur-val (double-array 1 start)
        counter (long-array 1 0)
        ^doubles out (create-buffer)
        len (alength out)]
    (fn ^doubles[]
      (let [cnt (getl counter)
            [last-sam increment] (get-line-pt cnt linedata)]
        (if (and last-sam increment) 
          (loop [end last-sam
                 incr increment 
                 v (getd cur-val)
                 c cnt
                 i 0]
            (if (< i len)
               (if (and end incr)
                (if (< c end)
                  (let [new-v (* v incr)]
                    (aset out i new-v)
                    (recur end incr new-v (unchecked-inc c) (unchecked-inc-int i)))
                  (let [[new-end new-incr] (get-line-pt c linedata)]
                    (recur new-end new-incr v c i)))
                (do 
                  (aset out i 0.0)
                  (recur nil nil 0.0 (unchecked-inc c) (unchecked-inc-int i))))
              (do
                (aset cur-val 0 v)
                (aset counter 0 c)
                out)))
          nil)))))


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
