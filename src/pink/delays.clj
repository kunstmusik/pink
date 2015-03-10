(ns pink.delays
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffer mix-buffers generator]]
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
        (recur (unchecked-inc indx) 
               (rem (unchecked-inc rw-ptr) delay-time)))
      (yield out))))

;(defn frac-delay
;  "Fixed length, interpolating delay-line. delay-time given in samples."
;  [afn ^double delay-time]
;  (generator
;      [rw-ptr (long 0)]
;      [sig afn]
;      (let [v (aget delay-buffer rw-ptr)]
;        (aset delay-buffer rw-ptr sig)
;        (aset out int-indx v)
;        (recur (unchecked-inc indx) 
;               (rem (unchecked-inc rw-ptr) delay-time)))
;      (yield out)))


(defn adelay
  "Non-interpolating delay-line with fixed delay-time. delay-time given in
  seconds."
  [afn ^double delay-time]
  (samp-delay afn (int (+ 0.5 (* delay-time (double *sr*))))))

;(defn fdelay
;  "Fractional delay. Time is given in seconds."
;  [afn ^double delay-time]
;  (let [out ^doubles (create-buffer) 
;        delay-buffer-len (int (+ 0.5 (* delay-time (double *sr*))))
;        delay-buffer (double-array delay-buffer-len 0.0)
;        read-ptr (int-array 1 *buffer-size*) ; start reading one buffer ahead
;        write-ptr (int-array 1 0)]
;    (fn []
;      (when-let [asig ^doubles (afn)]
;        (let [cur-read-ptr ^int (aget read-ptr 0)
;              cur-write-ptr ^int (aget write-ptr 0)
;              new-write-ptr
;              (do-write asig delay-buffer cur-write-ptr delay-buffer-len)
;              new-read-ptr 
;              (do-read out delay-buffer cur-read-ptr delay-buffer-len)]
;          (aset read-ptr 0 (int new-read-ptr))
;          (aset write-ptr 0 (int new-write-ptr))
;          out))))
;  )


;; Multi-Tap Delay

(defn create-delay
  ^doubles [^double delay-time-max]
  (double-array (int (+ 0.5 (* delay-time-max (double *sr*))))))

(defn delay-read
  [delay-buffer read-time]
  )

(defn delay-write
  [delay-buffer afn])


