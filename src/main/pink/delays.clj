(ns pink.delays
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffer mix-buffers generator gen-recur]])
  (:import [clojure.lang IFn$LD])
  )

;; Ensure unchecked math used for this namespace
(set! *unchecked-math* :warn-on-boxed)

;; feedback functions

(defn feedback-read 
  "Takes in a buffer and returns an audio function that will return that buffer.
   Pair with feedback-write to do feedback in signal graph. "
  [buffer]
  (fn []
    buffer))

(defn feedback-write
  "Writes afn result into a buffer as side-effect, returns afn result. Pair with
 feedback-read to do feedback in signal graph. "
  [afn buffer]
  (let [buffer-size (long *buffer-size*)] 
    (fn []
    (when-let [b (afn)]
      (System/arraycopy b 0 buffer 0 buffer-size) 
      b))))

;; sample read function

(defn delay-read-samp-i
  ^double [^doubles delay-buffer ^long write-ptr ^double delay-time]
  (let [delay-int (long delay-time)
        delay-frac1 (- delay-time delay-int)
        delay-frac0 (- 1.0 delay-frac1)
        delay-length (long (alength delay-buffer))]
    (let [indx0 (let [temp-indx (- write-ptr delay-int)]
                  (if (< temp-indx 0) (+ temp-indx delay-length) temp-indx))
          v0 (aget delay-buffer indx0)
          indx1 (let [temp-indx (- indx0 1)]
                  (if (< temp-indx 0) (+ temp-indx delay-length) temp-indx))
          v1 (aget delay-buffer indx1)
          v (+ (* delay-frac0 v0) (* delay-frac1 v1))]
      v)))

;; Delay utility functions

(defn create-delay
  ^doubles [^double delay-time-max]
  (double-array (int (+ 0.5 (* delay-time-max (double *sr*))))))

(defn delay-read
  ^clojure.lang.IFn$LD [^doubles delay-buffer ^long delay-time]
  (let [delay-length (long (alength delay-buffer))]
   (fn ^double [^long write-ptr]
      (let [indx (let [temp-indx (- write-ptr delay-time)]
                    (if (< temp-indx 0) (+ temp-indx delay-length) temp-indx))]
        (aget delay-buffer indx)))))

(defn delay-readi
  ^clojure.lang.IFn$LD [^doubles delay-buffer ^double delay-time]
  (let [delay-int (long delay-time)
        delay-frac1 (- delay-time delay-int)
        delay-frac0 (- 1.0 delay-frac1)
        delay-length (long (alength delay-buffer))]
    (fn ^double [^long write-ptr]
      (let [indx0 (let [temp-indx (- write-ptr delay-int)]
                    (if (< temp-indx 0) (+ temp-indx delay-length) temp-indx))
            v0 (aget delay-buffer indx0)
            indx1 (let [temp-indx (- indx0 1)]
                    (if (< temp-indx 0) (+ temp-indx delay-length) temp-indx))
            v1 (aget delay-buffer indx1)
            v (+ (* delay-frac0 v0) (* delay-frac1 v1))]
        v))))

;; simple adelay

(defn samp-delay
  "Non-interpolating delay-line with fixed-delay-time. delay-time given in
  samples."
  [afn ^long delay-time]
  (let [out (create-buffer) 
        ^doubles delay-buffer (double-array delay-time)
        rw-ptr (int-array 1 0)]
    (generator
      [rw-ptr (long 0)]
      [sig afn]
      (let [v (aget delay-buffer rw-ptr)]
        (aset delay-buffer rw-ptr sig)
        (aset out int-indx v)
        (gen-recur (rem (unchecked-inc rw-ptr) delay-time)))
      (yield out))))

(defn frac-delay
  "Linear interpolating (fractional) delay-line. delay-time given in samples."
  [afn ^double delay-time]
  (let [out (create-buffer) 
        delay-length (+ (long delay-time) 1)
        ^doubles delay-buffer (double-array delay-length)
        rw-ptr (int-array 1 0)
        ^IFn$LD del-read (delay-readi delay-buffer delay-time)]
    (generator
      [rw-ptr (long 0)]
      [sig afn]
      (let [v (.invokePrim del-read rw-ptr)]
        (aset delay-buffer rw-ptr sig)
        (aset out int-indx v)
        (gen-recur (rem (unchecked-inc rw-ptr) delay-length)))
      (yield out))))

(defn adelay
  "Non-interpolating delay-line with fixed delay-time. delay-time given in
  seconds."
  [afn ^double delay-time]
  (samp-delay afn (int (+ 0.5 (* delay-time (double *sr*))))))


(defn fdelay
  "Interpolating (fractional) delay-line with fixed delay-time. delay-time
  given in seconds."
  [afn ^double delay-time]
  (frac-delay afn (* delay-time (double *sr*))))


;;(defn delay-write
;  [delay-buffer afn]
  
;  )


