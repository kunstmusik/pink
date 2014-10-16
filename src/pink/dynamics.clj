(ns pink.dynamics
  "Functions for dealing with dynamics/amplitude of audio"
  (:require [pink.util :refer [create-buffer getd generator]]
            [pink.config :refer [*buffer-size* *sr*]]))

(def ^:const ^double LOG10D20
    (/  (Math/log 10) 20))

(defn db->amp
  "Convert decibel to power ratio"
  [^double d] 
  (Math/exp  (* d LOG10D20)))

(defn balance
  "Adjust one audio signal according to the values of another. 
  Based on Csound's balance opcode."
  ([asig acomp] (balance asig acomp 10))
  ([asig acomp ^double hp]
   {:pre (number? hp)}
   (let [TPIDSR (/ (* 2 Math/PI) (double *sr*))
         b (- 2.0 (Math/cos (* hp TPIDSR)))
         c2 (- b (Math/sqrt (- (* b b) 1.0)))
         c1 (- 1.0 c2)
         prvq (double-array 1 0.0)
         prvr (double-array 1 0.0)
         prva (double-array 1 0.0)
         out ^doubles (create-buffer)]

     ; this one needs some thought...
     ;(generator 
     ;  [prvq 0.0
     ;   prvr 0.0
     ;   prva 0.0]
     ;  [ain asig
     ;   cin acomp]


     ;  (yield out)
     ;  )


     (fn []
       (let [abuf ^doubles (asig)
             cbuf ^doubles (acomp)
             buf-size (long *buffer-size*)] 
       (when (and abuf cbuf)
         (loop [i (int 0)
                q (getd prvq)
                r (getd prvr)]
           (if (< i buf-size)
             (let [av (aget abuf i)
                   cv (aget cbuf i)]
               (recur 
                 (unchecked-inc-int i)
                 (+ (* c1 av av) (* c2 q))
                 (+ (* c1 cv cv) (* c2 r)))) 
             (do 
               (aset prvq 0 q)
               (aset prvr 0 r))))
         (let [q (getd prvq)
               r (getd prvr)
               a (if (zero? q)
                   (Math/sqrt r)
                   (Math/sqrt (/ r q)))
               pa (getd prva)
               diff (- a pa)
               ]
           (if (zero? diff)
             (loop [i 0] 
               (when (< i buf-size)
                 (aset out i (* a (aget abuf i)))
                 (recur (unchecked-inc-int i))))
             (let [incr (/ diff buf-size)]
               (loop [i 0 m pa]
                 (if (< i buf-size)
                   (do 
                     (aset out i (* m (aget abuf i)))
                     (recur (unchecked-inc-int i) (+ m incr)))  
                   (aset prva 0 a))
                 ))) 

           out)))))))

