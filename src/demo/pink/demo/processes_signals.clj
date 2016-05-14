(ns pink.demo.processes-signals
 (:require [pink.simple :refer :all]
             [pink.event :refer :all] 
             [pink.instruments.piano :refer :all]
             [pink.instruments.horn :refer :all]
             [pink.util :refer [mul try-func hold-until]]
             [pink.filters :refer :all]
             [pink.node :refer :all]
             [pink.space :refer :all]
             [pink.config :refer :all]
             [pink.envelopes :refer [env]]
             [pink.processes :refer [process wait cue countdown-latch] :as p]
             [pink.control :refer [chain]]
             ))

;; Study in Lutoslawski-style aleatory (i.e., ad libitum). Cues used by
;; conductor process to signal performer processes.  Latch used by conductor to
;; wait for each of the initial processes to complete at least one iteration
;; before waiting to give initial cue.

(defn instr 
  [amp key-num]
  (->
    (piano :duration *duration* :keynum key-num :amp amp)
    ;(mul (hold-until 0.5 1.0 (env [0.0 1.0 0.1 0.0])))
    (pan 0.0)
    ))

(defn perf-piano
  [start dur amp midi-key]
  (add-audio-events 
    (i instr start dur amp midi-key)))

(defn perf-until-cued
  [pitches durs cue]
  (process
    (loop []
      (when (not (p/has-cued? cue))
        (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
          (loop [[p & p-r] pitches [d & d-r] durs]
            (when (and p d)
              (let [dur (* d time-adj)]                  
                (when (not= p :rest) 
                  (perf-piano 0 dur 0.15 (+ 72 p)))
                (wait dur)
                (recur p-r d-r)))))
        (recur)
        ))))

(defn perf-until-cued-signal-latch
  [pitches durs cue latch]
  (process
        (loop [cued-latch false]
          (when (not (p/has-cued? cue)) 
            (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
                (loop [[p & p-r] pitches [d & d-r] durs]
                  (when (and p d)
                    (let [dur (* d time-adj)]                  
                      (when (not= p :rest) 
                        (perf-piano 0 dur 0.15 (+ 72 p)))
                      (wait dur))
                    (recur p-r d-r))))
              (when (not cued-latch)
                (p/count-down latch))
              (recur true)
            ))))

(defn transpose
  [v tr]
  (map #(if (number? %) (+ % tr) %) v))

;; SETUP PERFORMER PROCESSES

  (def cue0 (cue))
  (def cue1 (cue))
  (def latch0 (countdown-latch 3))

  (def p1-proc0 
    (perf-until-cued-signal-latch 
      [0 2 3 0 2 3 :rest 6]
      [0.15 0.15 0.15 0.15 0.15 0.15 0.8 2.0]
      cue0 latch0))

  (def p1-proc1
    (perf-until-cued 
      [0 3 0 3 7 :rest]
      [0.15 0.15 0.15 0.15 0.15 2.2]
      cue1))


  (def p2-proc0
    (perf-until-cued-signal-latch 
      (transpose [2 5 2 :rest 5 2 5 :rest] -24)
      [0.2 0.2 0.2 0.4 0.2 0.2 0.2 1.5 ]
      cue0 latch0))


  (def p2-proc1
    (perf-until-cued 
      (transpose [2 3 4 5 :rest] -24)
      [0.2 0.2 0.2 2.0 0.5]
      cue1))


  (def p3-proc0
    (perf-until-cued-signal-latch 
      (transpose [5 11 5 11 12 13 :rest] -12)
      [0.2 0.2 0.2 0.4 0.6 0.8 1.4 ]
      cue0 latch0))


  (def p3-proc1
    (perf-until-cued 
      (transpose [6 8 9 :rest] -12)
      [0.2 0.2 0.2 2.0]
      cue1))


  (def p1 (chain p1-proc0 p1-proc1))
  (def p2 (chain p2-proc0 p2-proc1))
  (def p3 (chain p3-proc0 p3-proc1))

  (def conductor 
    (process
      (add-pre-cfunc p1)
      (wait (+ 1 (Math/random)))
      (add-pre-cfunc p2)
      (wait (+ 1 (Math/random)))
      (add-pre-cfunc p3)
      (wait latch0)
      (wait 12.0)
      (p/signal-cue cue0)
      (wait 15.0)
      (p/signal-cue cue1)
      ))

(comment
  
  (start-engine)

  ;; Testing of PinkSignals


  (add-pre-cfunc conductor)

)
