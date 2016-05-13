(ns pink.processes
  "Write process code that works as control functions."
  (:require 
    [clojure.core.async.impl.ioc-macros :as ioc]
    [pink.config :refer :all]
    ))

;; EXPERIMENTAL CODE 

;; (defonce PinkProcessList)
;; (defn list-pink-processes [])
;; (kill-all-pink-processes [])

(defprotocol IPinkProcess
  (paused? [this])
  (active? [this])
  (toggle-pause [this])
  (kill [this]))

(deftype PinkProcess [^:volatile-mutable paused 
                      ^:volatile-mutable active 
                      proc-fn]
  IPinkProcess
  (paused? [this] paused)
  (active? [this] active)
  (toggle-pause [this ] (set! paused (not paused)))
  (kill [this] 
    (set! active false) :dead)
  clojure.lang.IFn  ;; used to conform to Pink's Control Function convention
  (invoke [this] 
    (cond
      (not active) false
      paused true
      :default (proc-fn) 
      )))

(defn create-pink-process 
  [proc-fn]
  (PinkProcess. false true proc-fn))

;; SIGNALS

(defprotocol PinkSignal
  (signal-done? [this]))

(defprotocol ICue
  (has-cued? [this]) 
  (signal-cue [this]))

(deftype Cue [^:volatile-mutable sig]
  ICue
  (has-cued? [this] sig)
  (signal-cue [this] (set! sig true))
  PinkSignal
  (signal-done? [this] (has-cued? this)))

(defn cue 
  "Create a cue signal that satisfies ICue and PinkSignal protocols.
  Useful for one-to-many signalling."
  []
  (Cue. false))

(defprotocol ICountdownLatch
  (count-down [this])
  (latch-done? [this]))

(deftype CountdownLatch [^:volatile-mutable num-wait]
  ICountdownLatch
  (count-down [this] (set! num-wait (dec num-wait)))
  (latch-done? [this] (= 0 num-wait))
  PinkSignal
  (signal-done? [this] (latch-done? this)))

(defn countdown-latch 
  "Create a countdown-latch that satisfies ICountdownLatch and
  PinkSignal protcols. Useful for coordinating and waiting for
  multiple processes to signal."
  [^long num-wait]
  (CountdownLatch. num-wait) )

;; PROCESS MACHINERY 

(def ^:const WAIT-IDX 6)

(defn pink-wait [c] c)

;; Surrounding my-wait with a loop will induce core.async's
;; state-machine macros to add a new block for just the call
;; to my-wait. When the state-machine returns, it will
;; operate just the wait code, rather than all of the code
;; prior to the wait terminator. 
(defmacro wait 
  "Wait upon a given time, PinkSignal, or predicate. Must be used
  within a Pink process."
  [c] 
  `(loop []
     (pink.processes/pink-wait ~c)))

(defn process-wait  [state blk val]
  (cond 
    (number? val)
    (let  [cur-wait  (long (ioc/aget-object state WAIT-IDX))
           ksmps  (long *buffer-size*)
           next-buf  (+ cur-wait ksmps)
           wait-time (long 
                       (Math/round 
                       (+ 0.499999 (* (double *sr*) (double val)))))]
      ;(println next-buf " : " wait-time)
      (if  (> next-buf wait-time)
        (do 
          (ioc/aset-all! state 
                         WAIT-IDX (rem next-buf wait-time) 
                         ioc/STATE-IDX blk) 
          :recur)
        (do 
          (ioc/aset-all! state WAIT-IDX next-buf)
          true)))
    (satisfies? PinkSignal val)
    (if (signal-done? val)
       (do 
          (ioc/aset-all! state WAIT-IDX 0 
                         ioc/STATE-IDX blk) 
          :recur)
       true) ;; pass through
    (fn? val)
    (let [v (val)] 
      (if v ;; function signals ready to move on 
         (do 
          (ioc/aset-all! state WAIT-IDX 0 
                         ioc/STATE-IDX blk) 
          :recur)
         true ;; pass through
        ))
    :default
    (throw (Exception. (str "Illegal argument: " val)))))

(defn process-done [state val]
  false)

(defmacro process
  "Create a state-machine-based Pink control function."
  [& body]
  (let  [terminators  {`pink-wait `process-wait
                       `counter `process-counter
                       :Return `process-done}]
    `(let  [captured-bindings# (clojure.lang.Var/getThreadBindingFrame) 
            state# (~(ioc/state-machine `(do ~@body) 1  
                                        (keys &env) 
                                        terminators))]
       ;; TODO - consider replacing WAIT-IDX to use a long-array to save on object allocations
       (ioc/aset-all! state#
                      WAIT-IDX 0
                      ;~ioc/BINDINGS-IDX  (clojure.lang.Var/getThreadBindingFrame)
                      )
       (fn []
         (ioc/aset-all! state# ~ioc/BINDINGS-IDX  (clojure.lang.Var/getThreadBindingFrame))
         (ioc/run-state-machine state#)))))



(comment

  (def p 
    (process
      (loop  [a 0]
        (when  (< a 5)
          (wait 0.1) 
          (println "test!")
          (recur  (inc a))))))

  (require '[clojure.pprint :refer [pprint]])
  (pprint (macroexpand 
            '(process
               (loop  [a 0]
                 (when  (< a 5)
                   (wait 3) 
                   (println "test!")
                   (recur  (inc a)))))

            ))

  (loop  [c 0]
    (let  [v  (p)]
      (println c " : " v)
      (when v
        (recur  (inc c)))))
  )
