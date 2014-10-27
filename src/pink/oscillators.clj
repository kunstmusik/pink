(ns pink.oscillators
  "Oscillator Functions"
  (:require [pink.config :refer [*sr* *buffer-size*]]
            [pink.util :refer [create-buffer arg generator]]
            [pink.gen :refer [gen-sine]] 
            ))

(def ^:const PI Math/PI)
(def ^:const TWO_PI (* 2 PI))

(defn phasor 
  "Phasor with fixed frequency and starting phase"
  [^double freq ^double phase]
  (let [phase-incr ^double (/ freq (double *sr*))
        ;cur-phase (double-array 1 phase)
        out ^doubles (create-buffer)]
    (generator 
      [cur-phase phase]
      []
      (do
        (aset out indx cur-phase)
        (recur (unchecked-inc indx) (rem (+ phase-incr cur-phase) 1.0)))
      (yield out))))

(defn sine 
  "Sine generator with fixed frequency and starting phase"
  ([^double freq]
   (sine freq 0.0))
  ([^double freq ^double phase]
   (let [phsr (phasor freq phase)
         out ^doubles (create-buffer)]
     (generator 
       [] [phs phsr]
       (let [v (Math/sin (* TWO_PI phs))]
         (aset out indx v) 
         (recur (unchecked-inc indx)))          
       (yield out)))))

;(require '[no.disassemble :refer :all])
;(println (disassemble sine))


(defn vphasor 
  "Phasor with variable frequency and fixed starting phase."
  [freq phase]
  {:pre (number? phase)}
  (let [out ^doubles (create-buffer)
        len (alength out)
        freq-fn (arg freq)]
    (generator [cur-phase phase]
               [f freq-fn]
               (if (<= f 0) 
                 (do 
                   (aset out indx Double/NEGATIVE_INFINITY)
                   (recur (unchecked-inc indx) cur-phase))
                 (let [incr ^double (/ f (long *sr*))]
                   (aset out indx cur-phase)
                   (recur (unchecked-inc indx) (rem (+ cur-phase incr) 1.0))))
               (yield out))))

(defn sine2 
  "Sine generator with variable frequency and fixed starting phase."
  ([f]
   (sine2 f 0))
  ([f p]
   (let [phsr (vphasor (arg f) p)
         out ^doubles (create-buffer)]
     (generator
       []
       [phase phsr]
       (if (= phase Double/NEGATIVE_INFINITY) 
         (do 
           (aset out indx 0.0)
           (recur (unchecked-inc indx)))
         (let [v (Math/sin (* TWO_PI phase))]
           (aset out indx v)
           (recur (unchecked-inc indx)) ))
       (yield out)))))

(def sine-table (gen-sine))

(defn oscil
  "Oscillator with table (defaults to sine wave table, truncates indexing)"
  ([amp freq]
   (oscil amp freq sine-table 0))
  ([amp freq table]
   (oscil amp freq table 0))
  ([amp freq ^doubles table phase]
   (let [phsr (vphasor (arg freq) phase)
         out ^doubles (create-buffer)
         tbl-len (alength table)
         ampfn (arg amp)]
     (generator 
       []
       [phase phsr
        amp ampfn]
       (if (= phase Double/NEGATIVE_INFINITY)
         (do 
           (aset out indx 0.0)
           (recur (unchecked-inc indx)))
         (let [v (* amp (aget table (int (* phase tbl-len))))]
           (aset out indx v)
           (recur (unchecked-inc indx))))
       (yield out)))))


(defn oscili
  "Linear-interpolating oscillator with table (defaults to sine wave table)"
  ([amp freq]
   (oscili amp freq sine-table 0))
  ([amp freq table]
   (oscili amp freq table 0))
  ([amp freq ^doubles table phase]
   (let [phsr (vphasor (arg freq) phase)
         out ^doubles (create-buffer)
         tbl-len (alength table)
         ampfn (arg amp)]
     (generator 
       [] 
       [p phsr 
        amp ampfn]
       (if (= p Double/NEGATIVE_INFINITY) 
         (do 
           (aset out indx 0.0)
           (recur (unchecked-inc indx)))
         (let [phs (* p tbl-len)
               pt0 (int phs)
               pt1 (mod (inc pt0) tbl-len)  
               frac (if (zero? pt0) 
                      phs
                      (rem phs pt0))
               v0  (aget table pt0)
               v1  (aget table pt1)
               v (* amp (+ v0 (* frac (- v1 v0))))]
           (aset out indx v)
           (recur (unchecked-inc indx))))
       (yield out)))))


(defn oscil3
  "Cubic-interpolating oscillator with table (defaults to sine wave table) (based on Csound's oscil3)"
  ([amp freq]
   (oscil3 amp freq sine-table 0))
  ([amp freq table]
   (oscil3 amp freq table 0))
  ([amp freq ^doubles table phase]
   (let [phsr (vphasor (arg freq) phase)
         ^doubles out (create-buffer)
         tbl-len (alength table)
         ampfn (arg amp)]
     (generator 
       [] 
       [p phsr 
        amp ampfn]
       (if (= p Double/NEGATIVE_INFINITY) 
         (do 
           (aset out indx 0.0)
           (recur (unchecked-inc indx)))
         (let [phs (* p tbl-len)
               pt1 (int phs)
               pt0 (if (zero? pt1) (- tbl-len 1) (- pt1 1))  
               pt2 (rem (inc pt1) tbl-len)  
               pt3 (rem (inc pt2) tbl-len)  
               x (if (zero? pt1) 
                   phs
                   (rem phs pt1))
               x2 (* x x)
               x3 (* x x2)
               p0  (aget table pt0)
               p1  (aget table pt1)
               p2  (aget table pt2)
               p3  (aget table pt3)
               a (/ (+ p3 (* -3 p2) (* 3 p1) (* -1 p0)) 6)                      
               b (/ (+ p2 (* -2 p1) p0) 2)
               c (+ (* p3 (double -1/6)) p2 (* p1 (double -1/2)) (* p0 (double -1/3)))
               d p1 
               v (* amp (+ (* a x3) (* b x2) (* c x) d))]
           (aset out indx v)
           (recur (unchecked-inc indx))))
       (yield out)))))

;; Implementation of Bandlimited Impulse Train (BLIT) functions by Stilson and
;; Smith. Based on implementations from Synthesis Toolkit (STK)

(defmacro calc-harmonics 
  [p nharmonics]
  `(if (<= ~nharmonics 0)
    (let [max-harmonics# (Math/floor (* 0.5 ~p))]
      (+ (* 2 max-harmonics#) 1))
    (+ (* 2 ~nharmonics) 1)))

(defmacro pi-limit
  [v]
  `(if (>= ~v Math/PI) (- ~v Math/PI) ~v))

(def DOUBLE-EPSILON
  (Math/ulp 1.0))

;; blit-saw

(defn- blit-saw-static
  [^double freq ^long nharmonics]
  (let [^doubles out (create-buffer)
        p (/ (long *sr*) freq)
        c2 (/ 1 p)
        rate (* Math/PI c2)
        m (long (calc-harmonics p nharmonics))
        a (/ m p)]
    (generator
      [phase 0.0
       st (* -0.5 a)]
      [] 
      (let [denom (Math/sin phase)
            tmp (+ (- st c2) 
                   (if (<= (Math/abs denom) ^double DOUBLE-EPSILON)
                     a
                     (/ (Math/sin (* m phase)) (* p denom))))
            new-st (* tmp 0.995)
            new-phs (pi-limit (+ phase rate))]
        (aset out indx tmp) 
        (recur (unchecked-inc indx) new-phs new-st)) 
      (yield out))))

(defn- blit-saw-dynamic
  [freq ^long nharmonics]
  (let [out ^doubles (create-buffer)
          initialized (atom false)]
    (generator
      [phase 0.0
       st 0.0]
      [f freq]
      (if (<= f 0)
        (do 
          (aset out indx 0.0)
          (recur (unchecked-inc indx) phase st))
        (let [denom (Math/sin phase)
              p (/ (long *sr*) f)
              c2 (/ 1 p)
              rate (* Math/PI c2)
              m (long (calc-harmonics p nharmonics))
              a (/ m p)
              st-val ^double (if @initialized 
                               st 
                               (reset! initialized (* -0.5 a)))
              tmp (+ (- ^double st-val c2) 
                     (if (<= ^double (Math/abs denom) ^double DOUBLE-EPSILON)
                       a
                       (/ (Math/sin (* m phase)) (* p denom))))
              new-st (* tmp 0.995)
              new-phs (pi-limit (+ phase rate))]
          (aset out indx tmp) 
          (recur (unchecked-inc indx) new-phs new-st)))(yield out))))

(defn blit-saw
  "Implementation of BLIT algorithm by Stilson and Smith for band-limited
  sawtooth waveform. Based on the C++ implementation from STK.
 
  Returns an optimized audio-function if freq is a number, or a slower
  version if freq is itself an audio-function."
  ([freq] (blit-saw freq 0))
  ([freq ^long nharmonics]
   {:pre [(or (and (number? freq) (pos? freq)) (fn? freq))] }
  (if (number? freq)
    (blit-saw-static (double freq) nharmonics)    
    (blit-saw-dynamic freq nharmonics) 
    )))



;; blit-square

(def TOP_LIM (- TWO_PI 0.1))
         
(defmacro calc-square-harmonics 
  [p nharmonics]
  `(if (<= ~nharmonics 0)
    (let [max-harmonics# (int (Math/floor (* 0.5 ~p)))]
      (* 2 (+ max-harmonics# 1)))
    (* 2 (+ ~nharmonics 1))))

(defmacro two-pi-limit
  [v]
  `(if (>= ~v TWO_PI) (- ~v TWO_PI) ~v))

(defn- blit-square-static
  [^double freq ^long nharmonics]
  (let [out ^doubles (create-buffer)
        p (/ (* 0.5 (long *sr*)) freq)
        rate (/ Math/PI p)
        m (calc-square-harmonics p nharmonics)
        a (/ m p) ]
    (generator 
      [phase 0.0
       last-val 0.0
       last-blit 0.0]
      []
      (let [denom (Math/sin phase)
            new-blit (+ last-blit 
                        (if (< (Math/abs denom) ^double DOUBLE-EPSILON)
                          (if (or (< phase 0.1) (> phase ^double TOP_LIM))
                            a
                            (- a))
                          (/ (Math/sin (* m phase)) (* p denom))))
            new-val (+ new-blit (- last-blit) (* 0.999 last-val)) ; dc blocked
            new-phs (two-pi-limit (+ phase rate))]
        (aset out indx new-val) 
        (recur (unchecked-inc indx) new-phs new-val new-blit))
      (yield out))))

(defn- blit-square-dynamic
  [freq ^long nharmonics]
  (let [out ^doubles (create-buffer)]
    (generator
      [phase 0.0
       last-val 0.0
       last-blit 0.0]
      [f freq]
      (if (<= f 0) 
        (do 
          (aset out indx 0.0)
          (recur (unchecked-inc indx) phase last-val last-blit))
        (let [p (/ (* 0.5 (long *sr*)) f)
              rate (/ Math/PI p)
              m ^long (calc-square-harmonics p nharmonics)
              a (/ m p) 
              denom (Math/sin phase)
              new-blit (+ last-blit 
                          (if (< (Math/abs denom) ^double DOUBLE-EPSILON)
                            (if (or (< phase 0.1) (> phase ^double TOP_LIM))
                              a
                              (- a))
                            (/ (Math/sin (* m phase)) (* p denom))))
              new-val (+ new-blit (- last-blit) (* 0.999 last-val)) ; dc blocked
              new-phs (two-pi-limit (+ phase rate))]
          (aset out indx new-val) 
          (recur (unchecked-inc indx) new-phs new-val new-blit)))
      (yield out))))


(defn blit-square
  "Implementation of BLIT algorithm by Stilson and Smith for band-limited
  square waveform. Based on the C++ implementation from STK.
 
  Returns an optimized audio-function if freq is a number, or a slower
  version if freq is itself an audio-function."
  ([freq] (blit-square freq 0))
  ([freq ^long nharmonics]
   {:pre [(or (and (number? freq) (pos? freq)) (fn? freq))] }
  (if (number? freq)
    (blit-square-static (double freq) nharmonics)    
    (blit-square-dynamic freq nharmonics) 
    )))
