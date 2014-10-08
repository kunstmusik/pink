(ns pink.delays
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffer]]
            )
  )

(defn- do-write
  [^doubles asig ^doubles delay-buffer 
   ^long write-ptr ^long delay-buffer-len ]
  (let [new-end (+ write-ptr *buffer-size*)]
   (if (>= new-end delay-buffer-len)
    (let [size0 (- delay-buffer-len write-ptr 1)
          size1 (- *buffer-size* size0)] 
      (System/arraycopy asig 0 delay-buffer write-ptr size0)
      (System/arraycopy asig size0 delay-buffer 0 size1)
      size1)
    (do 
      (println write-ptr " " new-end " " delay-buffer-len " " *buffer-size* " " (alength delay-buffer) " " asig)
      (System/arraycopy asig 0 delay-buffer write-ptr *buffer-size*)
      new-end))))

(defn- do-read
  [^doubles out ^doubles delay-buffer 
   ^long read-ptr ^long delay-buffer-len ]
  (let [new-end (+ read-ptr *buffer-size*)]
   (if (>= new-end delay-buffer-len)
    (let [size0 (- delay-buffer-len read-ptr 1)
          size1 (- *buffer-size* size0)] 
      (System/arraycopy delay-buffer read-ptr out 0 size0)
      (System/arraycopy delay-buffer 0 out size0 size1)
      size1)
    (do 
      (System/arraycopy delay-buffer read-ptr out 0 *buffer-size*)
      new-end))))

(defn adelay
  "Fixed length, non-interpolating delay-line. delay-time given in seconds."
  [afn delay-time]
  (let [out ^doubles (create-buffer) 
        delay-buffer-len (int (+ 0.5 (* delay-time *sr*)))
        delay-buffer (double-array delay-buffer-len 0.0)
        read-ptr (int-array 1 *buffer-size*) ; start reading one buffer ahead
        write-ptr (int-array 1 0)]
    (fn []
      (when-let [asig ^doubles (afn)]
        (let [cur-read-ptr ^int (aget read-ptr 0)
              cur-write-ptr ^int (aget write-ptr 0)
              new-write-ptr
              (do-write asig delay-buffer cur-write-ptr delay-buffer-len)
              new-read-ptr 
              (do-read out delay-buffer cur-read-ptr delay-buffer-len)]
          (aset read-ptr 0 (int new-read-ptr))
          (aset write-ptr 0 (int new-write-ptr))
          out)))))

