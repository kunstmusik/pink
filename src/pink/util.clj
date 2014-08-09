(ns pink.util
  "Audio utility code for working with buffers (double[])"
  (:require [pink.config :refer [*ksmps* *current-buffer-num* *sr*]])
  (:import [java.util Arrays]))

;(defn getd ^double [^doubles a] (aget a 0))
;(defn setd! ^double [^doubles a ^double v] (aset a 0 v))

(defn tagit 
  [a t]
  (with-meta a {:tag t}))

(defn tag-doubles 
  [a]
  (tagit a "doubles"))

(defn tag-double
  [a]
  (tagit a "double"))

(defn tag-longs
  [a]
  (tagit a "longs"))

(defn tag-long
  [a]
  (tagit a "long")
  )

(defmacro getd
  ([a]
  `(aget ~(tag-doubles a) 0)))

(defmacro setd! 
  [a v] 
  `(aset ~(tag-doubles a) 0 ~(tag-double v)))


(defmacro getl 
  [a] 
  `(aget ~(tag-longs a) 0))

(defmacro setl! 
  [a v] 
  `(aset ~(tag-longs a) 0 ~(tag-long v)))


(defmacro swapd! [d f] 
  `(setd! ~d (~f (getd ~d))))

(defmacro swapl! [l f]
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
  (let [my-buf-num (long-array 1 -1)
        buffer (atom nil) ]
    (fn []
      (if (not= (getl my-buf-num) *current-buffer-num*)
        (do 
          (setl! my-buf-num *current-buffer-num*)
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

(defmacro map-d-impl
  [out f & buffers]  
  (let [cnt (gensym 'count)
        get-bufs (map (fn [a] (list 'aget a cnt)) buffers )
        apply-line `(~f ~@get-bufs)
        ] 
    `(when (and ~@buffers)
     (let [l# (alength ~out)]
       (loop [~cnt (unchecked-int 0)]
         (when (< ~cnt l#)
           (aset ~out ~cnt
                  ~(tag-double apply-line)) 
           (recur (unchecked-inc-int ~cnt))
           ))
       ~out
       )    
     )))

(defn map-d 
  "Maps function f across double[] buffers and writes output to out buffer" 
  ([^doubles out f ^doubles x]
    (map-d-impl out f x)   
   )
  ([^doubles out f ^doubles x ^doubles y ]
    (map-d-impl out f x y)   
   )
  ([^doubles out f ^doubles x ^doubles y  ^doubles z]
    (map-d-impl out f x y z)   
   )
  ([^doubles out f ^doubles x ^doubles y  ^doubles z ^doubles a]
    (map-d-impl out f x y z a)   
   )
  ([^doubles out f ^doubles x ^doubles y  ^doubles z ^doubles a ^doubles b]
    (map-d-impl out f x y z a b)   
   )
  ([^doubles out f ^doubles x ^doubles y  ^doubles z ^doubles a ^doubles b 
    ^doubles c]
    (map-d-impl out f x y z a b c)   
   )
  )
        
(defmacro fill 
  "Fills double[] buf with values. Initial value is set to value from double[1] start, 
  then f called like iterate with the value.  Last value is stored back into the start.
  Returns buf at end."
  [out start f]
  (let [cnt (gensym 'count)]
    `(when (and ~out ~start ~f)
       (let [len# (alength ~(tag-doubles out))]
         (loop [~cnt (unchecked-int 0)]
           (when (< ~cnt len#)
             (aset ~(tag-doubles out) ~cnt (swapd! ~start ~f))
             (recur (unchecked-inc-int ~cnt))))
         ~(tag-doubles out)))))  

(defn- gen-buffer [x] (x))

(defn- operator 
  "takes in func and list of generators funcs, map operator across the result buffers"
  [f a]
  (let [args (map arg a)]
    (if (> (count args) 1)
      (let [out (create-buffer)]
        (fn ^doubles []
          (let [buffers (map gen-buffer args) ]
            (when (not-any? nil? buffers)
              (apply map-d out f buffers)))))
      (nth args 0))))

(defn mul [& a]
  (operator * a))

(defn div [& a]
  (operator / a))

(defn sum 
  [& a]
  (operator + a))

(defn mix
  [& xs]
  (let [args (map arg xs)]
    (if (> (count args) 1)
      (let [tmp (create-buffer)
            out (create-buffer)
            adjust (create-buffer (/ 1.0 (count args)))]
        (fn ^doubles []
          (let [buffers (map (fn [a] (a)) args)]
           (map-d out * adjust (apply map-d tmp + buffers)))))
      (nth args 0))))


(defn with-duration 
  [dur afn]
  (let [end (long (/ (* dur *sr* *ksmps*))) 
        cur-buffer (long-array 1 0)]
    (fn []
      (let [v (aget cur-buffer 0)] 
        (if (< v end)
          (do 
            (aset cur-buffer 0 (inc v))
            (afn)) 
        )) 

    )))

(defmacro with-ksmps
  "Run code with given ksmps. Uses binding to bind *ksmps* during 
  initialization-time as well as performance-time. Returns an audio function
  that will appropriately fill a buffer of *ksmps* size with repeated calls 
  to the code of ksmps size."
  [ksmps & bindings] 
  (let [buf-sym (gensym)
        out-buf-sym (gensym)
        ]
    `(if (zero? (rem *ksmps* ~ksmps))
       (let [frames# (/ *ksmps* ~ksmps)
             ~out-buf-sym (create-buffer)
             done# (atom false)]
         (binding [*ksmps* ~ksmps] 

           (let [afn# (binding [*ksmps* ~ksmps]
                        ~@bindings)]
             (fn [] 
               (if @done#
                 nil
                 (loop [i# 0] 
                   (if (< i# frames#)
                     (let [~buf-sym (afn#)] 
                       (if ~buf-sym 
                         (do 
                           (System/arraycopy ~buf-sym 0 
                                             ~out-buf-sym (* i# ~ksmps) 
                                             ~ksmps)
                           (recur (unchecked-inc-int i#)))
                         (do
                           (reset! done# true)
                           (when (not (zero? i#))
                             (Arrays/fill ~(tag-doubles out-buf-sym) 
                                          (* i# ~ksmps) (* frames# ~ksmps) 0.0) 
                             ~out-buf-sym))))
                     ~out-buf-sym)))))))
       (throw (Exception. (str "Invalid ksmps: " ~ksmps))))) )


;; Informal benchmarking tool

(defn time-gen 
  [gen]
  (let [t (. System nanoTime)
      ]
      (loop [cnt (unchecked-int 0)]
        (if (nil? (gen))
          (println (format "TIME: %g FRAMES: %d" 
                           (double (/ (- (. System nanoTime) t) 1000000000))
                           cnt))
          (recur (unchecked-inc-int cnt)))))
  )
