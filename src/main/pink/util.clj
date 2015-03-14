(ns pink.util
  "Audio utility code for working with buffers (double[])"
  (:require [pink.config :refer [*buffer-size* *current-buffer-num* *sr*]]
            [primitive-math :refer [not==]])
  (:import [java.util Arrays]
           [pink Operator]
           [clojure.lang IFn IDeref]))

;; Dynamic argument resolution
(deftype DynamicArg [func args])

(defn apply!*!
  ([func] (func))
  ([func args]
   (->> (if (sequential? args) args [args])
        (map #(if (instance? IDeref %)
                (deref %)
                %))
        (apply func)))
  ([func arg & args]
   (apply!*! func (list* arg args))))

(defn !*! 
  "Returns a reference (instance of IDeref) that when deref'd, will call the 
  given function and args. Note that apply!*! is used to call the function
  with the arguments, so arguments that are instances of IDeref (i.e. atoms)
  will first be derefenced before the function is applied."
  [func & args]
  (reify IDeref
    (deref [_] (apply!*! func args))))

(defn !r! 
  "Returns an IDeref that when derefed, returns the r-argument.  Used for
  passing atoms and other references as arguments for events, so that the event
  function will receive the atom/ref itself and not its value.  (This is due to
  events using apply!*!, which derefs any args before applying its function.)"
  [r]
  (reify IDeref
    (deref [_] r)))


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
  (tagit a "long"))

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
           (recur (unchecked-inc ~cnt))
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

;; Functions for working with atoms 

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
  (^doubles [] (double-array *buffer-size*))
  (^doubles [^double i] (double-array *buffer-size* i)))

(defn create-buffers
  "Creates a single-channel or multi-channel buffer"
  [nchnls]
  (if (= 1 nchnls)
    (create-buffer)
    (into-array 
      (for [_ (range nchnls)] (create-buffer)))))

(def ^{:tag 'doubles} EMPTY-BUFFER (create-buffer 0)) 

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
  (let [^long src-count (buffer-channel-count src)
        ^long dest-count (buffer-channel-count dest)]
    (if (= src-count dest-count 1)
      (Operator/sum ^doubles dest ^doubles src)
      (cond 
        (= src-count 1) 
        (Operator/sum ^doubles (aget ^"[[D" dest 0) ^doubles src)

        (= dest-count 1) 
        (Operator/sum ^doubles dest ^doubles (aget ^"[[D" src 0))

        :else
        (loop [i 0 end (min src-count dest-count)]
          (when (< i end)
            (Operator/sum ^doubles (aget ^"[[D" dest i) 
                          ^doubles (aget ^"[[D" src i))
            (recur (unchecked-inc i) end))))))
  dest)

(defn clear-buffer 
  [b]
  (if (multi-channel? b)
    (loop [i (int 0) cnt (count b)]
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
      (let [cur-buf (long *current-buffer-num*)] 
        (if (not== (getl my-buf-num) cur-buf )
          (do 
            (aset my-buf-num 0 cur-buf)
            (reset! buffer (afn))) 
          @buffer)))))

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
             (recur (unchecked-inc ~cnt))))
         ~(tag-doubles out)))))  

(defn- gen-buffer [x] (x))

(defmacro operator 
  "takes in func and list of generators funcs, map operator across the result buffers"
  [f a]
  ;(let  [args  (map arg a)]
  ;  (if  (>  (count args) 1)
  ;    (let  [out  (create-buffer)]
  ;      (fn ^doubles  []
  ;        (let  [buffers  (map gen-buffer args) ]
  ;          (when  (not-any? nil? buffers)
  ;            (apply map-d out f buffers)))))
  ;    (nth args 0)))
  (let [out (with-meta (gensym "out") {:tag 'doubles})
        buf (with-meta (gensym "out") {:tag 'doubles})]
   `(if (> (count ~a) 1)
    (let [~out (create-buffer)
          args# (map arg ~a)
          buffer-size# (unchecked-int *buffer-size*)
          fns# ^"[Lclojure.lang.IFn;" (into-array IFn args#)
          fun_len# (alength fns#)]
      (fn ^{:tag 'doubles} []
        (when-let [first-buf# ((aget fns# 0))]
          (System/arraycopy first-buf# 0 ~out 0 buffer-size#)
          (loop [i# 1]
            (if (< i# fun_len#) 
              (when-let [~buf ((aget fns# i#))]
                (loop [j# (unchecked-int 0)]
                  (when (< j# buffer-size#)
                    (aset ~(tag-doubles out) j# 
                          (~f (aget ~out j#) 
                              (aget ~buf j#)))
                    (recur (unchecked-inc j#))))
                (recur (unchecked-inc i#)))  
              ~out)))))
    (nth ~a 0))))

(defn mul2 [& a] (operator * a))
(defn div2 [& a] (operator / a))
(defn add2 [& a] (operator + a))
(defn sub2 [& a] (operator - a))

(defmacro native-operator
  [f a]
  (let [out (tag-doubles (gensym "out"))
        afns (with-meta (gensym "afns") {:tag "[Lclojure.lang.IFn;"})
        ] 
    `(let [~out (create-buffer) 
        ~afns (into-array IFn (map arg ~a))]
    (fn []
      (~f ~out ~afns)))))

(defn mul 
  [& a]
  (native-operator Operator/mul a))

(defn div 
  [& a]
  (native-operator Operator/div a))

(defn sum 
  [& a]
  (native-operator Operator/sum a))

(defn sub 
  [& a]
  (native-operator Operator/sub a))

;; Macro for Generators

(defmacro box-val [v]
  (into-array Double/TYPE [v]))

(defn- process-bindings [bindings]
  {:pre (even? bindings)}
  (reduce 
    (fn [[x y z] [b c]]
      (let [state-sym (gensym "state")] 
        (if (and (sequential? c) (= 'long (first c)))
         [(conj x state-sym (list 'long-array 1 (second c) ))
          (conj y b `(aget ~(tag-longs state-sym) 0))
          (conj z `(aset ~(tag-longs state-sym) 0 ~b))] 
         [ (conj x state-sym (list 'double-array 1 c))
          (conj y b `(aget ~(tag-doubles state-sym) 0))
          (conj z `(aset ~(tag-doubles state-sym) 0 ~b))] 
          )
        )) 
    [[] [] []]
    (partition 2 bindings)))

(defn- handle-yield [bindings ret-sym]
  {:pre (even? bindings)}
  `(do 
     ~@bindings 
     ~ret-sym))

(defn- process-afn-bindings
  [afn-bindings]
  (reduce
    (fn [[x y z] [b c]]
      (let [bsym (gensym "buffer")] 
        [(conj x bsym (with-meta (list c) {:tag "doubles"}))
         (conj y bsym)
         (if (vector? b)
           (loop [out z 
                  [sig & sigs] b 
                  channel 0]
             (if sig
               (recur (conj out sig (list 'aget (with-meta bsym {:tag "[[D"}) channel 'int-indx)) sigs (inc channel))
               out))
           (conj z b (list 'aget (tag-doubles bsym) 'indx))   
           )]))
    [[] [] []] (partition 2 afn-bindings)))

(defn ^:private gen-outer-body
  [new-afn-bindings afn-results inner-body]
  (if (pos? (count new-afn-bindings))
    `(let [~@new-afn-bindings]
       (when (and ~@afn-results)
         ~inner-body))
    inner-body))

(defmacro generator 
  "Creates an audio-function. 
  * Bindings are for values that will be automatically saved and loaded between calls
  * afn-bindings are for setting var name to use when indexing a sample from the buffer generated
  by the audio-function
  * body should do processing and recur with newly calculated values
  * yield-form should be (yield value-to-return)"
  [bindings afn-bindings body yield-form] 
  (let [indx-sym 'indx
        int-indx-sym 'int-indx 
        [new-afn-bindings afn-results
          afn-indexing] (process-afn-bindings afn-bindings)
        [state new-bindings save-bindings] (process-bindings bindings) 
        yield-body (handle-yield save-bindings (second yield-form))
        bsize-sym (gensym "buffer-size")
        inner-body `(loop [~indx-sym 0 
                           ~@new-bindings]
                      (if (< ~indx-sym ~bsize-sym)
                        (let [~int-indx-sym (int ~indx-sym) 
                              ~@afn-indexing] 
                          ~body )          
                        ~yield-body 
                        ))
        generator-body (gen-outer-body new-afn-bindings afn-results inner-body)]
    `(let [~@state
           ~bsize-sym (long *buffer-size*)] 
       (fn [] ~generator-body))))

(defmacro gen-recur
  [& args]
  (let [indx-sym 'indx]
  `(recur (unchecked-inc ~indx-sym) ~@args)))

;(generator 
;  [a (long 4)
;   b 3.5]
;  []
;  (recur (unchecked-inc a) (+ 1.5 b))
;  (yield a))

;; functions for processing

(defn duration-processor
  [^double dur ^booleans done-arr afn]
  (let [end (long (/ (* dur (double *sr*))
                     (double *buffer-size*)))
        cur-buffer (long-array 1 0)]
    (fn []
      (let [v (aget cur-buffer 0)] 
        (if (>= v end)
          (aset done-arr 0 true)
          (aset cur-buffer 0 (unchecked-inc v)))) 
      (afn))))

;; TODO - the calculation of duration needs to be done according to future tempo
;; if time-varying tempo is used. May need to introduce tempo-mapper to event-list. 
(defmacro with-duration 
  [dur body]
  `(let [done-arr# (boolean-array 1 false)
         adjusted-dur# (* ~dur (/ 60.0 (double pink.config/*tempo*)))] 
     (binding [pink.config/*duration* adjusted-dur#
             pink.config/*done* done-arr#]
      (let [afn# ~body]
        (duration-processor adjusted-dur# done-arr# afn#)))))

(defn is-done?
  [done]
  (aget ^booleans done 0))

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
             current-buf-num# (long-array 1 0)]
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
                           (recur (unchecked-inc i#)
                                  (unchecked-inc buf-num#)))
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

(defprotocol 
  Allocator
  (acquire-alloc! [x] "Returns true if alloc is available. Note: may update current alloc num when called.")
  (num-allocs [x] "Returns number of current allocs.")
  (reset-allocs! [x] "Reset allocs to 0.")
  (with-allocator [x afn] "Decorates afn with dealloc-on-done afn, which decrements alloc count when afn is done."))

(defn dealloc-on-done 
  [atm afn]
  (fn []
    (if-let [b (afn)]
      b
      (locking atm
        (swap! atm (fn [^long a] (if (pos? a) (dec a) 0)))
        nil))))

;; This might not be the best use of protocol/reify... revisit this later, could always be
;; swapped out with a drop-in replacement of functions that take in an allocator data struct (or atom)
(defn create-max-allocator
  "Keeps track of current allocations for limiting max-number. Warning: Depends on audio functions
  exiting by returning nil to keep track of deallocations. ."
  [^long max-allocs]
  (let [allocs (atom 0)]
    (reify Allocator
      (num-allocs [x] @allocs)
      (acquire-alloc! [x] 
        (locking allocs
          (let [^long v @allocs]
            (if (< v max-allocs)
              (do 
                (swap! allocs inc)
                true)
              false))))
        (reset-allocs! [x]
          (locking allocs 
            (reset! allocs 0)))
        (with-allocator [x afn]
          (dealloc-on-done allocs afn)))))


