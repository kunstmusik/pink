(ns pink.audio.util
  "Audio utility code for working with buffers (double[])"
  (:require [pink.audio.engine :refer [*ksmps* *current-buffer-num*]]
            [hiphip.double :as dbl]
            [hiphip.array :as arr]
            ))

(defn getd ^double [^doubles a] (aget a 0))
(defn setd! ^double [^doubles a ^double v] (aset a 0 v))

(defn getl ^long [^longs a] (aget a 0))
(defn setl! ^long [^longs a ^long v] (aset a 0 v))


;(defn ^double swapd! [d f] 
;  (setd! d (f (getd d))))

(definline swapd! [d f] 
  `(dbl/aset ~d 0 (~f (dbl/aget ~d 0))))

;(defn ^long swapl! [l f]
;  (setl! l (f (getl l))))

(definline swapl! [l f]
  `(setl! ~l (~f (getl ~l))))

(defn create-buffer 
  ([] (double-array *ksmps*))
  ([i] (double-array *ksmps* i)))


(defn const 
  "Initializes a *ksmps*-sized buffer with the given value,
  returns a function that will return that buffer on each call"
  [^double a] 
  (let [out (create-buffer a)]
  (fn ^doubles []
    out)))

(defn arg
  "Utility function to pass through if it is a function, or
  wrap within a const if it is a number"
  [a]
  (if (number? a)
    (const (double a))
    a))


(defn shared 
  "Wraps an audio function so that it only generates values once per ksmps block; uses 
  *curent-buffer-num* dynamic variable to track if update is required" 
  [afn] 
  (let [my-buf-num (atom -1)
        buffer (atom nil) ]
    (fn []
      (if (not= @my-buf-num *current-buffer-num*)
        (do 
          (reset! my-buf-num *current-buffer-num*)
          (reset! buffer (afn))) 
        @buffer))))

(defn- decorate-shared 
  "Utility function for let-s macro to decorated bindings with (shared)"
  [args] 
  (reduce 
      (fn [a [b c]] 
        (conj (conj a b) (list `shared c)))
        [] 
      (partition 2 args)))


(defmacro let-s
  "Macro that decorates bindings with (shared) to simplify instrument building."
  [bindings & body]
  `(let ~(decorate-shared bindings)
     ~@body))

(comment

"This code here needs to be moved to a unit test..."

(decorate-shared '[e #(+ 1 2)])
(macroexpand-1 
  '(let-s [e #(+ 1 2)] 
     (println "test3")))
  
  )

(defn reader 
  "Returns function that reads from atom and returns a buffer. Useful for mutable data derived from external source such as MIDI or OSC"
  [atm] 
  (let [last (atom 0)
        buffer (atom (create-buffer))]
    (fn []
      (when (not= @atm @last)
         (reset! buffer (create-buffer @atm))
         (reset! last @atm))
      @buffer)))


(def empty-d (create-buffer 0)) 

(defn clear-d [^doubles d]
  (when d
    (let [len (min (alength ^doubles d) (alength ^doubles empty-d))]
    (System/arraycopy empty-d 0 d 0 len))))

(defn map-d 
  "Maps function f across double[] buffers and writes output to final passed in buffer" 
  ([f ^doubles a ^doubles b]
    (when (and a b)
      (let [l (alength a)]
        (loop [cnt 0]
          (when (< cnt l)
            (dbl/aset b cnt (f (dbl/aget a cnt)))
            (recur (unchecked-inc cnt))))
        b)))
  ([f ^doubles a ^doubles b ^doubles c]
    (when (and a b c)
      (let [l (alength a)]
        (loop [cnt 0]
          (when (< cnt l)
            (aset c cnt ^double (f (aget a cnt) (aget b cnt)))
            (recur (unchecked-inc cnt))))
        c))))

(defn reduce-d
  "calls f on buffers generates from fns in a manner similar to reduce, 
  writing the reduced values into out buffer"
  ([f ^doubles out fns]
    (when (and out fns (not-empty fns))
      (clear-d out)
      (loop [[x & xs] fns]
        (if x
          (when-let [buf ^doubles (x)]
            (let [len (alength buf)]
              (when buf
                (loop [cnt 0]
                  (when (< cnt len) 
                    (aset out cnt ^double (f (aget out cnt) (aget buf cnt)))
                    (recur (unchecked-inc cnt))))
                (recur xs))))
          out)))))
        
(defn fill 
  "Fills double[] buf with values. Initial value is set to value from double[] start, 
  then f called like iterate with the value.  Last value is stored back into the start.
  Returns buf at end."
  [^doubles buf ^doubles start f]
  (when (and buf start f)
    (let [len (alength buf)
          lastindx (dec len)]
      (loop [cnt (unchecked-long 0)]
        (when (< cnt len)
          (aset ^doubles buf cnt ^double (swapd! start f))
          (recur (unchecked-inc cnt))))
      buf)))


(defn ^doubles mul-d [^doubles a ^doubles b ^doubles out]
  (when (and a b)
    (dbl/amap [x a y b] (* x y))))

;;(defmacro op [opfn & args])

;(defn mul [a b]
;  (let [ ;out (create-buffer)
;        af (arg a)
;        bf (arg b)]
;    (fn ^doubles []
;      (let [as (af) 
;            bs (bf)]
;        (when (and as bs) 
;          (dbl/amap [v0 as v1 bs] (* v0 v1)))))))

(defn mul [& a]
  (let [args (map arg a)]
    (fn ^doubles []
      (let [buffers (map (fn [a] (a)) args)]
        (when (not-any? nil? buffers) 
          (reduce #(dbl/amap [x %1 y %2] (* x y)) buffers))))))

(defn mix
  [& xs]
  (let [args (map arg xs)]
    (if (> (count args) 1)
      (let [tmp (create-buffer)
            out (create-buffer)
            adjust (create-buffer (/ 1.0 (count args)))]
        (fn ^doubles []
          (mul-d adjust (reduce-d + tmp args) out)))
      (nth args 0))))


(defn sum 
  [& xs]
  (let [args (map arg xs)]
    (if (> (count args) 1)
      (let [tmp (create-buffer)
            out (create-buffer)]
        (fn ^doubles []
          (reduce-d + tmp args)))
      (nth args 0))))
