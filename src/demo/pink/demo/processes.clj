(ns pink.demo.processes
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
             ))

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

(comment
  
  (start-engine)


  (add-pre-cfunc
    (process
      (loop [a 0]
        (when (< a 32)
          (perf-piano 0 1 0.15 (+ 60 (* 12 (Math/random)))) 
          (wait 0.25)
          (recur (inc a))
          ))))

  (add-pre-cfunc
    (process
      (loop [a 0]
        (when (< a 32)
          (let [pitch (+ 60 (* 12 (Math/random)))]
            (perf-piano 0 1 0.15 pitch) 
            (perf-piano 0 1 0.15 (+ 7 pitch))) 
          (wait 0.25)
          (recur (inc a))
          ))))

  (add-pre-cfunc
    (process
      (loop [a 0]
        (when (< a 32)
          (let [pitch (+ 48 (* 12 (Math/random)))]
            (perf-piano 0 3 0.25 pitch) 
            )
          (wait 3)
          (recur (inc a))
          ))))

  (add-pre-cfunc
    (process
      (loop [a 0 brownian (Math/random)]
        (when (< a 16)
          (let [pitch (+ 48 (* 12 (Math/random)))
                dur (+ 4 brownian)]
            (perf-piano 0 dur 0.25 pitch)
            (wait dur))
          (recur (inc a) (mod (+ (Math/random) brownian ) 1.0))
          ))))

  (add-pre-cfunc
    (process
      (loop [a 0 brownian (Math/random)]
        (when (< a 32)
          (let [pitch (+ 75 (* 12 (Math/random)))
                dur (+ 0.2 brownian)]
            (perf-piano 0 dur 0.25 pitch)
            (wait dur))
          (recur (inc a) (mod (+ (Math/random) brownian ) 1.0))
          ))))

  (add-audio-events
    (i instr 0.0 4.0 0.25 60))
  
  (add-audio-events
    (i instr 0.0 4.0 0.25 48))

  (add-audio-events
    (i instr 0.0 8.0 0.3 36))

)

