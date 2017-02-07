(ns pink.live-code
  "Functions for live coding music using pink.simple engine."
  (:require [pink.simple :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer :all]
            [pink.util :refer :all]
            [pink.node :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]
            [pink.effects.reverb :refer :all]
            [pink.io.sound-file :refer :all]
            [clojure.string :refer [join]]
            ) 
  )

;; One-shot Sample Player 

(defn sample-duration 
  "Return duration of audio sample"
  [wave]
  (let [d ^doubles (aget ^"[[D"(:data wave) 0)]
    ;; TODO - *sr* may require replacement with an (sr) function
    ;; to simplify getting sr value from pink.simple engine
    (/ (alength d) (double *sr*)))) 

(defn sample-one-shot
  "Returns audio function that plays a stereo sample without looping until
  completion."
  ([sample]
   (let [dur (sample-duration sample)]
     (mul (env [0 0 0.001 0.5 (- dur 0.002) 0.5 0.001 0])
          (oscili 1.0 (/ 1.0 dur) 
                  (aget ^"[[D" (:data sample) 0))
          ))))

;; event/time functions

(defn cause [func start & args]
  "Implementation of Canon-style cause function. Will create and schedule an
  event that calls the given function at given start time (in beats) and with
  given arguments."
  (add-events (apply event func start args)))

(defmacro redef! 
  "Macro to redefine given function 'a' to function value held in new-func.
  Useful in conjunction with cause to schedule when a function will be
  redefined. For example, when modifying a temporally recursive function, one
  can use (cause #(redef! a b) (next-beat 16)) to schedule that the function 'a'
  be redefined with value of 'b' at the next 4-bar boundary. Users may then
  edit body of function separately from its use in performance."
  [a new-func] 
  `(alter-var-root (var ~a) (fn [f#] ~new-func)))

(defmacro kill-recur! 
  "Macro to redefine given function name to a var-arg, no-op function. Useful
  to end temporal recursion of an event function that may be already queued up
  in the event list. 
  
  When using kill-recur!, be aware that the function def in memory no longer
  represents what is on screen. User will need to re-evaluate the function
  definition before using again in temporal recursion.

  When multiple instances of a function are in temporal recursion, kill-recur!
  may be used to stop all instances.
 
  User may schedule a call to kill-recur! by using wrapper function, such as:

  (cause #(kill-recur! my-perf-func) (next-beat 4)) 

  Note: Implemented as macro to work with var special-form."
  [a]
  `(redef! ~a (fn [& args#])))

(defmacro end-recur! 
  "Macro to redefine given function name to a var-arg function that
  short-circuits current operation.  Useful to end temporal recursion of an
  event function that may be already queued up in the event list. 
  
  end-recur! operates by swapping out the function value held in 'a' with one
  that will restore the value of 'a' to its previous value. This ends the
  temporal recursion of the function as well as allows the user start a new
  temporal recursion without first re-evaluating the 'a' function from the
  editor. This is in contrast to kill-recur! which will leave the function
  value as a no-op function. 
  
  end-recur! is best used when a single instance of a function is used in
  temporal recursion. If multiple recursions are in operation, end-recur! may
  not end all instances. 
 
  User may schedule a call to end-recur! by using wrapper function, such as:

  (cause #(end-recur! my-perf-func) (next-beat 4)) 

  Note: Implemented as macro to work with var special-form."
  [a]
  `(alter-var-root (var ~a) 
           (fn [f#] 
             (fn [& args#]
                (alter-var-root 
                  (var ~a) 
                  (fn [g#] f#))))))


(defn next-beat 
  "Calculates forward time for the next beat boundary.  Useful for scheduling
  of temporally recursive event function so that it will fire in sync with
  other beat-oriented functions.Adjusts for fractional part of current beat
  time that arises due to block-based processing. 

  For example, if an engine has a current beat time of 81.2, if (next-beat 4)
  is used, it will provide a value of 3.8, so that the event function will fire
  at beat 84."
  (^double [] (next-beat (now) 1.0))
  (^double [b] (next-beat (now) b))
  (^double [cur-beat-time b]
           (let [beat cur-beat-time 
                 base (Math/floor (double (/ (+ beat (* 0.05 b)) b)))]
             (double (- (* (inc base) b) beat)))))
 
(defn beat-advance
  "Calculates start time for x number of beats ahead in time.  Uses next-beat
  with subdiv time to get the adjusted time for one subdivision, then adds
  remaining number of units to subdiv. Units should be a whole number.
  
  For example, to calculate 7 16th notes ahead in time, use (beat-advance 1/4
  7)."
  ([subdiv] (beat-advance subdiv 1.0))
  ([subdiv units]
  (+ (next-beat subdiv) (* subdiv (dec units)))))

(defn beat-mod 
  "Returns modulus division of given beat-time and mod-val. Rounds to whole
  number beats. Defaults to using (now) if only mod-val is given. 2-arity
  version useful with multiplied versions of (now) to get sub-divisions of
  beat."
  ([mod-val] (beat-mod (now) mod-val))
  ([beat-time mod-val]
  (long (Math/round (double (mod beat-time mod-val))))))


(defn beats 
  "Returns time in seconds for num beats given. Useful for converting times
  appropriate for audio functions."
  [b]
  (beats->seconds b (tempo)))

(defn bars
  "Returns time in seconds for num bars given. Useful for converting times
  appropriate for audio functions."
  ([b]
   (bars b 4))
  ([b beats-per-bar]
   (beats (* b beats-per-bar))))


(defn sub-beat 
  "Returns current sub-beat time."
  [n]
  (* (now) n))


(defn cosr
  "Cosine that scales/shifts value into range given by low/high.
  Useful when generating events. Phase can be calculated using beat-phase.
  (Based on cosr by Andrew Sorensen)."
  ^double [^double phs ^double low ^double high]
  (let [r (- high low)
        c (Math/cos (* phs (* 2 Math/PI)))]
    (+ low (* r (* 0.5 (+ 1.0 c))))))

(defn beat-phase 
  "Returns current phase (0.0,1.0] of *beat* within total number of sub-beats. For example,
 using a sub-beat of 4 (i.e., 16th notes), 16 total would equal one measure at 
 4/4 time signature. Useful when paired with cosr."
  ^double [^double sub-div ^double total]
  (/ (beat-mod (sub-beat sub-div) total) total))



;; Sequences and Atoms

(defn next-in-atom 
  "Retrieves head of sequence held in atom and resets atom to rest of sequence.
  Not thread safe in that outside mutator may write a value that gets
  overwritten by the reset! call, but largely okay for the kinds of operations
  done in live-coding."
  [atm]
  (let [[f & r] @atm] 
    (reset! atm r)
    f))

(defn reset!!
  "Same as reset! but returns nil. Useful for live coding so that value from
  reset! does not try to get printed by editor's clojure plugin. For example,
  when using an atom to store an infinite sequence used by a recursive process,
  reset!! might cause an OutOfMemoryException as the editor would try to print
  the results of the reset!."
  [a b]
  (reset! a b)
  nil)



;; visualization

(defn beat-printer 
  "Temporally recursive function to print out beat-mod
  times.  Useful for seeing where one is within the current
  beat/bar structure."
  [& args]
  (when (pos? (count args))
    (let [fmt (join " " (repeat (count args) "%2d"))]
      (print "\r")
      (apply printf fmt (map beat-mod args))
      (.flush *out*)
      (cause beat-printer (next-beat) args)
      )))
