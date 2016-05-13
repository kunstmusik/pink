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


  ;; Testing of PinkSignals

  (def cue0 (cue))
  (def cue1 (cue))
  (def latch0 (countdown-latch 2))

  (def p1 
    (let [pitches0 [0 3 0 3 :rest]
          durs0 [0.2 0.2 0.2 0.8 2.0]
          pitches1 [0 3 0 3 7 :rest]
          durs1 [0.2 0.2 0.2 0.2 1.0 2.2]]
      (process
        (loop [cued-latch false]
          (cond 
            (not (p/has-cued? cue0)) 
            (do 
              (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
                (loop [[p & p-r] pitches0 [d & d-r] durs0]
                  (when (and p d)
                    (let [dur (* d time-adj)]                  
                      (when (not= p :rest) 
                        (perf-piano 0 dur 0.25 (+ 72 p)))
                      (wait dur))
                    (recur p-r d-r))))
              (when (not cued-latch)
                (p/count-down latch0))
              (recur true))
            (not (p/has-cued? cue1))
            (do 
              (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
                (loop [[p & p-r] pitches1 [d & d-r] durs1]
                  (when (and p d)
                    (let [dur (* d time-adj)]                  
                      (when (not= p :rest) 
                        (perf-piano 0 d 0.25 (+ 72 p)))
                      (wait d)
                      (recur p-r d-r)))))
              (recur true)))
          ))))

(def p2 
  (let [pitches0 [2 3 4 :rest]
        durs0 [0.3 0.3 1.0 1.0]
        pitches1 [2 3 4 5 :rest]
        durs1 [0.2 0.2 0.2 2.0 0.5]]
    (process
      (loop [cued-latch false]
        (cond 
          (not (p/has-cued? cue0)) 
          (do 
            (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
              (loop [[p & p-r] pitches0 [d & d-r] durs0]
                (when (and p d)
                  (let [dur (* d time-adj)]                  
                    (when (not= p :rest) 
                      (perf-piano 0 dur 0.25 (+ 48 p)))
                    (wait dur))
                  (recur p-r d-r))))
            (when (not cued-latch)
              (p/count-down latch0))
            (recur true))

          (not (p/has-cued? cue1))
          (do 
            (let [time-adj (+ 1 (* 0.2 (Math/random)))] 
              (loop [[p & p-r] pitches1 [d & d-r] durs1]
                (when (and p d)
                  (let [dur (* d time-adj)]                  
                    (when (not= p :rest) 
                      (perf-piano 0 dur 0.25 (+ 48 p)))
                    (wait dur))
                  (recur p-r d-r))))
            (recur true))
          
          )))))

(def conductor 
  (process
    (add-pre-cfunc p1)
    (wait (+ 1 (Math/random)))
    (add-pre-cfunc p2)
    (wait latch0)
    (wait 10.0)
    (p/signal-cue cue0)
    (wait 15.0)
    (p/signal-cue cue1)
    )
  )

  (add-pre-cfunc conductor)

)
