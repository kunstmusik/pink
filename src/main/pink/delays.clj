(ns pink.delays
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffer mix-buffers generator gen-recur]]
            ))

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

;; simple adelay

(defn- do-write
  [^doubles asig ^doubles delay-buffer 
   ^long write-ptr ^long delay-buffer-len ]
  (let [buf-size (long *buffer-size*)
        new-end (+ write-ptr buf-size)]
   (if (>= new-end delay-buffer-len)
    (let [size0 (- delay-buffer-len write-ptr 1)
          size1 (- buf-size size0)] 
      (System/arraycopy asig 0 delay-buffer write-ptr size0)
      (System/arraycopy asig size0 delay-buffer 0 size1)
      size1)
    (do 
      (System/arraycopy asig 0 delay-buffer write-ptr *buffer-size*)
      new-end))))

(defn- do-read
  [^doubles out ^doubles delay-buffer 
   ^long read-ptr ^long delay-buffer-len ]
  (let [buf-size (long *buffer-size*)
        new-end (+ read-ptr buf-size)]
   (if (>= new-end delay-buffer-len)
    (let [size0 (- delay-buffer-len read-ptr 1)
          size1 (- buf-size size0)] 
      (System/arraycopy delay-buffer read-ptr out 0 size0)
      (System/arraycopy delay-buffer 0 out size0 size1)
      size1)
    (do 
      (System/arraycopy delay-buffer read-ptr out 0 buf-size)
      new-end))))

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
        delay-int (long delay-time)
        delay-frac1 (- delay-time delay-int)
        delay-frac0 (- 1.0 delay-frac1)
        delay-length (+ delay-int 1)
        ^doubles delay-buffer (double-array delay-length)
        rw-ptr (int-array 1 0)]
    (generator
      [rw-ptr (long 0)]
      [sig afn]
      (let [indx0 (let [temp-indx (- rw-ptr delay-int)]
                    (if (< temp-indx 0) (+ temp-indx delay-length) temp-indx))
            v0 (aget delay-buffer indx0)
            indx1 (let [temp-indx (- indx0 1)]
                    (if (< temp-indx 0) (+ temp-indx delay-length) temp-indx))
            v1 (aget delay-buffer indx1)
            v (+ (* delay-frac0 v0) (* delay-frac1 v1))]
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


;; Multi-Tap Delay

(defn create-delay
  ^doubles [^double delay-time-max]
  (double-array (int (+ 0.5 (* delay-time-max (double *sr*))))))

(defn delay-read
  [delay-buffer read-time]
  )

(defn delay-write
  [delay-buffer afn])


