(ns pink.util
  "Audio utility code for working with buffers (double[])"
  (:require [pink.config :refer [*buffer-size* *current-buffer-num* *sr*]])
  (:import [java.util Arrays]))

;; utility for running audio-funcs and control-funcs

(defmacro try-func
  "Trys to call a function, returns the func's return value or
  nil if an exception was caught."
  [f]
  `(try 
    ~f
    (catch Exception e# 
      (.printStackTrace e#)
      nil)))


;; utility functions for tagging vars (useful for macros)

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

;; map-d 

(defmacro map-d-impl
  [out f & buffers]  
  (let [cnt (gensym 'count)
        get-bufs (map (fn [a] (list 'aget a cnt)) buffers )
        apply-line `(~f ~@get-bufs) ] 
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

;; Functions used with single-item double and long arrays
;; (Single item arrays are used to carry state between audio-function calls)

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

;; Functions for working with refs

(defmacro drain-atom!
  [a]
  `(loop [v# @~a]
    (if (compare-and-set! ~a v# [])
      v#
      (recur @~a))))

(defmacro concat-drain!
  [v r]
  `(if (empty? @~r) ~v (concat ~v (drain-atom! ~r))))

;; Functions related to audio buffers

(defn create-buffer 
  "Creates a single-channel audio buffer with optional default value"
  ([] (double-array *buffer-size*))
  ([i] (double-array *buffer-size* i)))

(defn create-buffers
  "Creates a single-channel or multi-channel buffer"
  [nchnls]
  (if (= 1 nchnls)
    (create-buffer)
    (into-array (take nchnls (repeatedly create-buffer)))))

(def EMPTY-BUFFER (create-buffer 0)) 

(def MULTI-CHANNEL-TYPE 
  (type (into-array [(double-array 1) (double-array 1)])))

(defmacro multi-channel?
  "Returns if buffer is multi-channel"
  [buffer]
  `(= MULTI-CHANNEL-TYPE (type ~buffer)))

(defmacro buffer-channel-count
  "Get the channel count for a buffer"
  [buffer]
  `(if (multi-channel? ~buffer) (count ~buffer) 1 ))

(defn mix-buffers
  "Mix src audio buffer into dest audio buffer, taking into account 
  differences in channel counts"
  [src dest]
  (let [src-count (buffer-channel-count src)
        dest-count (buffer-channel-count dest)]
    (if (= src-count dest-count 1)
      (map-d dest + dest src)
      (cond 
        (= src-count 1) (let [out (aget ^"[[D" dest 0)] (map-d out + out src)) 

        (= dest-count 1) (map-d dest + dest (aget ^"[[D" src 0)) 

        :else
        (loop [i 0 end (min src-count dest-count)]
          (when (< i end)
            (let [out (aget ^"[[D" dest i)]
              (map-d out + out (aget ^"[[D" src i))
              (recur (unchecked-inc i) end)))))))
  dest)

(defn clear-buffer 
  [b]
  (if (multi-channel? b)
    (loop [i 0 cnt (count b)]
      (when (< i cnt)
        (Arrays/fill ^doubles (aget ^"[[D" b i) 0.0)
        (recur (unchecked-inc i) cnt)))
    (Arrays/fill ^doubles b 0.0)))


;; Utility audio-functions

(defn const 
  "Initializes a *buffer-size*-sized buffer with the given value,
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
  "Wraps an audio function so that it only generates values once per buffer-size block; uses 
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

(defn mul 
  [& a]
  (operator * a))

(defn div 
  [& a]
  (operator / a))

(defn sum 
  [& a]
  (operator + a))


(defn with-duration 
  [dur afn]
  (let [end (long (/ (* dur *sr* *buffer-size*))) 
        cur-buffer (long-array 1 0)]
    (fn []
      (let [v (aget cur-buffer 0)] 
        (if (< v end)
          (do 
            (aset cur-buffer 0 (inc v))
            (afn)) 
        )) 

    )))

(defmacro with-buffer-size
  "Run code with given buffer-size. Uses binding to bind *buffer-size* during 
  initialization-time as well as performance-time. Returns an audio function
  that will appropriately fill a buffer of *buffer-size* size with repeated calls 
  to the code of buffer-size size."
  [buffer-size & bindings] 
  (let [buf-sym (gensym)
        out-buf-sym (gensym)
        ]
    `(if (zero? (rem *buffer-size* ~buffer-size))
       (let [frames# (int (/ *buffer-size* ~buffer-size))
             ~out-buf-sym (create-buffer)
             done# (atom false)
             current-buf-num# (long-array 1 0)
             ]
         (binding [*buffer-size* ~buffer-size] 

           (let [afn# (binding [*buffer-size* ~buffer-size]
                        ~@bindings)]
             (fn [] 
               (if @done#
                 nil
                 (loop [i# 0 
                        buf-num# (aget current-buf-num# 0)] 
                   (if (< i# frames#)
                     (let [~buf-sym (binding [*current-buffer-num* buf-num#] 
                                      (afn#))] 
                       (if ~buf-sym 
                         (do 
                           (System/arraycopy ~buf-sym 0 
                                             ~out-buf-sym (* i# ~buffer-size) 
                                             ~buffer-size)
                           (recur (unchecked-inc-int i#)
                                  (unchecked-inc-int buf-num#)))
                         (do
                           (reset! done# true)
                           (aset current-buf-num# 0 
                                 (+ (aget current-buf-num# 0) frames#))
                           (when (not (zero? i#))
                             (Arrays/fill ~(tag-doubles out-buf-sym) 
                                          (* i# ~buffer-size) (* frames# ~buffer-size) 0.0) 
                             ~out-buf-sym))))
                     (do
                       (aset current-buf-num# 0 buf-num#)
                       ~out-buf-sym))))))))
       (throw (Exception. (str "Invalid buffer-size: " ~buffer-size))))))


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
