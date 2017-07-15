(ns pink.instruments.pluck
  "Implementations of Karplus-Strong algorithm for plucked strings."

  (:require [pink.util :refer :all]
            [pink.config :refer [*buffer-size* *current-buffer-num* *sr*]]
            [pink.envelopes :refer :all]
            [pink.gen :refer [gen9 gen17]]
            [pink.oscillators :refer :all]
            [pink.filters :refer [tone atone]]
            [pink.delays :refer :all]
            [diff-eq.core :refer [dfn]])
  (:import [clojure.lang IFn$LD IFn$DD]))

;; Ensure unchecked math used for this namespace
(set! *unchecked-math* :warn-on-boxed)

(defn ss-one-pole
  [^double pole ^double gain]
  (let [b0 (if (> pole 0.0) (- 1.0 pole) (+ 1.0 pole))
        a1 (- pole)]
    (dfn [samp]
         y (- (* b0 (* samp gain))
              (* a1 [y -1])))))

(defn- ss-one-zero
  [^double zero]
  (let [b0 (if (> zero 0.0) 
             (/ 1.0 (+ 1.0 zero))
             (/ 1.0 (- 1.0 zero)))
        b1 (* (- zero) b0)]
    (dfn [samp]
         y (+ (* b1 [samp -1])
              (* b0 samp)))))

(defn phase-delay-one-zero
  ^double [^double freq ^double zero]
  (let [b0 (if (> zero 0.0) 
             (/ 1.0 (+ 1.0 zero))
             (/ 1.0 (- 1.0 zero)))
        b1 (* (- zero) b0)
        omegaT (/ (* 2.0 Math/PI freq) (double *sr*))
        real (+ (* b0 (Math/cos 0.0))
                (* b1 (Math/cos omegaT))) 
        imag (- 0.0 
                (* b0 (Math/sin 0.0))
                (* b1 (Math/sin omegaT)))
        phase (rem (Math/atan2 real imag) (* 2.0 Math/PI))]
    (/ phase omegaT)))

(defn- create-noise-buffer
  ^doubles [^double amp ^long len]
  (let [^doubles b (double-array len)
        pick-filter (ss-one-pole
                      (- 0.999 (* amp 0.15))
                      (* 0.5 amp))]
    (loop [indx 0]
      (when (< indx len)
        (aset b indx ^double (pick-filter (Math/random)))
        (recur (inc indx))))
    b))


;; TODO - factor in done-gain

(defn pluck
  "Basic Karplus-Strong implementation based on Plucked
  class from STK.
 
  amp - overall amplitude of pluck 
  freq - frequency in hertz
  "
  [^double amp ^double freq]
  (let [out (create-buffer)
        delay-time (- (/ (double *sr*) freq) 
                        (phase-delay-one-zero freq -1.0)) 
        ^doubles delay-buffer 
        (create-noise-buffer amp 
                             (int (+ 0.5 delay-time)))
        delay-length (long (alength delay-buffer))
        rw-ptr (int-array 1 0)
        ^IFn$LD del-read (delay-readi delay-buffer 
                                      delay-time)
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


