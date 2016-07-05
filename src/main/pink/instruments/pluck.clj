(ns pink.instruments.pluck
  "Implementations of Karplus-Strong algorithm for plucked strings."

  (:require [pink.util :refer :all]
            [pink.config :refer [*buffer-size* *current-buffer-num* *sr*]]
            [pink.envelopes :refer :all]
            [pink.gen :refer [gen9 gen17]]
            [pink.oscillators :refer :all]
            [pink.filters :refer [tone atone]]
            [pink.delays :refer :all]
            )
  (:import [clojure.lang IFn$LD]))

(defn- create-noise-buffer
  ^doubles [amp len]
  (let [^doubles b (create-delay (* 2 len))
        l (alength b) ]
    (loop [indx 0]
      (when (< indx l)
        (aset b indx (* amp (Math/random)))
        (recur (inc indx))))
    b))

(defn karplus 
  "Basic Karplus-Strong implementation.
 
  amp - overall amplitude of pluck
  freq - frequency in hertz 
  tau - time for decay of string in seconds
  "
  [^double amp ^double freq ^double tau]
  (let [out (create-buffer)
        ^doubles delay-buffer (create-noise-buffer amp (/ 1.0 freq))
        delay-time (/ 2.0 freq)
        delay-length (alength delay-buffer)
        end-samp (long (+ 1.0 (* tau *sr*))) 
        rw-ptr (int-array 1 0)
        pole (tau2pole (/ tau delay-length 4))
        ^IFn$LD del-read (delay-readi delay-buffer delay-time)
        ]
    (generator
      [rw-ptr (long 0)
       counter (long 0)]
      []
      (if (and (= 0 int-indx) (>= counter end-samp)) 
        nil 
        (let [v (* (.invokePrim del-read rw-ptr) pole)]
          (aset delay-buffer rw-ptr v)
          (aset out int-indx v)
          (gen-recur (rem (unchecked-inc rw-ptr) delay-length)
                     (unchecked-inc counter))))
      (yield out))))


