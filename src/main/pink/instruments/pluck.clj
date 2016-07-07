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
  (:import [clojure.lang IFn$LD IFn$DD]))

(defn ss-one-pole
  [^double pole ^double gain]
  (let [b0 (if (> pole 0.0) (- 1.0 pole) (+ 1.0 pole))
        a1 (- pole)
        ^doubles last-v (double-array 1 0.0)
        ]
    (fn ^double [^double samp]
      (let [v (- (* b0 (* samp gain))
                 (* a1 (aget last-v 0)))]
        (aset last-v 0 v)
        v))))

(defn- ss-one-zero
  [^double zero]
  (let [b0 (if (> zero 0.0) 
             (/ 1.0 (+ 1.0 zero))
             (/ 1.0 (- 1.0 zero)))
        b1 (* (- zero) b0)
        ^doubles last-v (double-array 1 0.0)]
    (fn ^double [^double samp]
      (let [v (+ (* b1 (aget last-v 0))
                 (* b0 samp))]
        (aset last-v 0 samp)
        v))))

(defn- create-noise-buffer
  ^doubles [^double amp len]
  (let [^doubles b (create-delay len)
        l (alength b) 
        pick-filter (ss-one-pole
                      (- 0.999 (* amp 0.15))
                      (* 0.5 amp))]
    (loop [indx 0]
      (when (< indx l)
        (aset b indx ^double (pick-filter (Math/random)))
        (recur (inc indx))))
    b))

;; TODO - add phase delay calculation to delay-line length

(defn plucked 
  "Basic Karplus-Strong implementation based on Plucked
  class from STK.
 
  amp - overall amplitude of pluck 
  freq - frequency in hertz
  "
  [^double amp ^double freq]
  (let [out (create-buffer)
        delay-time (/ 1.0 freq)
        ^doubles delay-buffer 
        (create-noise-buffer amp delay-time)
        delay-length (alength delay-buffer)
        rw-ptr (int-array 1 0)
        ^IFn$LD del-read (delay-readi delay-buffer delay-time)
        init-loop-gain (Math/min 
                         0.99999 
                         (+ 0.995 (* freq 0.000005))) 
        done-gain (- 1.0 amp)
        ^IFn$DD loop-filter (ss-one-zero -1.0)]
    (generator
      [rw-ptr (long 0)
       counter (long 0)
       loop-gain init-loop-gain]
      []
      (let [new-v (.invokePrim del-read rw-ptr)
            v (.invokePrim loop-filter (* new-v loop-gain))]
        (aset delay-buffer rw-ptr v)
        (aset out int-indx (* 3.0 v))
        (gen-recur 
          (rem (unchecked-inc rw-ptr) delay-length)
          (unchecked-inc counter)
          loop-gain))
      (yield out))))


