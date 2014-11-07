(ns pink.envelopes
  "Envelope Generator Functions"
  (:require [clojure.pprint :refer [pprint]]
            [pink.config :refer :all]
            [pink.util :refer :all]
            [primitive-math :refer [not==]])
  (:import [java.util Arrays])
  )

(defn- make-env-data 
  "Takes in a list of time tagged pairs (time value) and function for
  calculating the cached value and then creates a list with initial value,
  followed by a list of pairs of (num-samples, cached-value). opfn should
  take in [end start run]"
  [pts opfn]
  {:pre (even? (count pts))}
  (let [pairs (partition 2 pts)
        [x & xs] (if (not== (double (ffirst pairs)) 0.0)
                   (cons [0.0 (second (first pairs))] pairs)
                   pairs)]
     
    (cons (second x) 
          (second (reduce 
              (fn [[[a b] lst] [^double c ^double d :as p]] 
                (let [run (double (* c (double *sr*)))
                      rise (double (opfn d b run))
                      last-pt (last lst) 
                      total-run (if (nil? last-pt) 0.0 ^double (first last-pt))] 
                      [p (conj lst [(+ total-run run) rise])] ))
                         [x []] xs)))))

(defn get-line-pt 
  [^long sample linedata]
  (loop [[x & xs] linedata]
    (when x
      (if (< sample ^double (first x))
        x
        (recur xs)))))

(defn env
  "Generates an envelope given pairs of values (t0, v0, t1, v1 ...) where tx is duration of segment."
  [pts] 
  {:pre (even? (count pts))}
  (let [[start & linedata] (make-env-data pts #(/ (- ^double %1 ^double %2) ^double %3))
        cur-val (double-array 1 start)
        counter (long-array 1 0)
        ^doubles out (create-buffer)
        len (alength out)]
    (fn ^doubles[]
      (let [cnt (getl counter)
            [^double last-sam ^double increment] (get-line-pt cnt linedata)]
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
                    (recur end incr new-v (unchecked-inc c) (unchecked-inc i)))
                  (let [[new-end new-incr] (get-line-pt c linedata)]
                    (recur new-end new-incr v c i)))
                (do 
                  (aset out i 0.0)
                  (recur nil nil 0.0 (unchecked-inc c) (unchecked-inc i))))
              (do
                (aset cur-val 0 v)
                (aset counter 0 c)
                out)))
          nil)))))


;; EXPONENTIAL ENVELOPE

(defn- adjust-for-zero
  "for exponential envelopes, can not have zero"
  [^double x]
  (if (zero? x)
    0.0000000001
    x))

(defn- make-exp-env-data [pts]
  {:pre (even? (count pts))}
  (let [adjusted-pts 
        (map #(if (even? %2) %1 (adjust-for-zero %1)) pts (range))]
   (make-env-data adjusted-pts #(Math/pow (/ ^double %1 ^double %2) (/ 1.0 ^double %3)))))


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
    ;(generator
    ;  [counter 0]

    ;  )
    (fn ^doubles[]
      (let [cnt (getl counter)
            [^double last-sam ^double increment] (get-line-pt cnt linedata)]
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
                    (recur end incr new-v (unchecked-inc c) (unchecked-inc i)))
                  (let [[new-end new-incr] (get-line-pt c linedata)]
                    (recur new-end new-incr v c i)))
                (do 
                  (aset out i 0.0)
                  (recur nil nil 0.0 (unchecked-inc c) (unchecked-inc i))))
              (do
                (aset cur-val 0 v)
                (aset counter 0 c)
                out)))
          nil)))))


;; Simple Envs

(defn- adsr-impl
  "Linear ADSR that checks for *done* flag before doing release."
  [^double a ^double d ^double s ^double r]
  (let [^doubles out (create-buffer)
        done *done*
        sr (double *sr*)
        buffer-size (long *buffer-size*)
        attack-samps (long (* a sr))
        attack-incr (/ 1.0 attack-samps)
        decay-samps (long (* d sr))
        decay-incr (/ (- s 1.0) decay-samps)
        release-samps (long (* r sr))
        release-incr (/ (- s) release-samps)
        ^longs last-sample (long-array 1 0) 
        ^doubles last-val (double-array 1 0.0)
        stage (atom :attack)
        ]
    (fn []
      (if (= @stage :complete)
        nil

        (do
          (when (and (is-done? done) (not= :release @stage))
            (aset last-sample 0 0)
            (reset! stage :release))
          (loop [indx 0 
                 last-v (aget last-val 0)
                 cur-samp (aget last-sample 0)]
            (if (< indx buffer-size) 
              (do 
                (aset out indx last-v)
                (case @stage

                  :attack
                  (if (< cur-samp attack-samps)
                    (recur (unchecked-inc indx) (+ last-v attack-incr) (unchecked-inc cur-samp)) 
                    (do
                      (reset! stage :decay)
                      (recur (unchecked-inc indx) 1.0 0)))              

                  :decay
                  (if (< cur-samp decay-samps)
                    (recur (unchecked-inc indx) (+ last-v decay-incr) (unchecked-inc cur-samp)) 
                    (do
                      (reset! stage :sustain)
                      (recur (unchecked-inc indx) s 0)))              

                  :sustain
                  (do 
                    (Arrays/fill out indx buffer-size s)
                    (recur buffer-size s 0))

                  :release 
                  (let [v (+ last-v release-incr)]
                    (if (< v 0)
                      (do 
                        (Arrays/fill out indx buffer-size 0.0)
                        (reset! stage :complete)
                        (recur buffer-size 0.0 0))
                      (recur (unchecked-inc indx) v (unchecked-inc cur-samp))))
                  ))
              (do 
                (aset last-val 0 last-v)
                (aset last-sample 0 cur-samp)
                out))))
        ))))   

(defn adsr 
  "Linear Attack-Decay-Sustain-Release Envelope. If *done* boolean array flag is used,
  will await until done is set to true before performing release stage.  Otherwise, 
  defaults to *duration* or 1.0 for total time of envelope."
  [^double a ^double d ^double s ^double r ]
  (let [dur *duration*
        done *done*]
    (cond 
      done
      (adsr-impl a d s r)     

      dur
      (env [0.0 0.0 a 1.0 d s (max 0.0 (- dur a d r)) s r 0.0])

     :else
      (env [0.0 0.0 a 1.0 d s 1.0 s r 0.0])
      )
    )
  )

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
