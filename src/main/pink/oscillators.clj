(ns pink.oscillators
  "Oscillator Functions"
  (:require [pink.config :refer [*sr* *buffer-size*]]
            [pink.util :refer [create-buffer arg generator shared mul sub gen-recur]]
            [pink.gen :refer [gen-sine]])
  (:import [clojure.lang IFn$DD]))

(set! *unchecked-math* true)

(def ^:const ^:private ^{:tag 'double} PI Math/PI)
(def ^:const ^:private ^{:tag 'double} TWO_PI (* 2.0 PI))

(defn vphasor 
  "Phasor with variable frequency and variable starting phase."
  [freq phase]
  (let [out ^doubles (create-buffer)
        len (alength out)
        freq-fn (arg freq)
        phase-fn (arg phase)
        sr (double *sr*)]
    (generator 
      [cur-phase 0.0]
      [f freq-fn
       phs phase-fn]
      (if (<= f 0.0) 
        (do 
          (aset out int-indx Double/NEGATIVE_INFINITY)
          (gen-recur cur-phase))
        (let [incr (/ f sr)]
          (aset out int-indx (rem (+ cur-phase phs) 1.0))
          (gen-recur (+ cur-phase incr))))
      (yield out)))) 

(defn phasor-fixed
  "Phasor with fixed frequency and starting phase"
  [^double freq ^double phase]
  (let [phase-incr ^double (/ freq (double *sr*))
          out ^doubles (create-buffer)]
      (generator 
        [cur-phase phase]
        []
        (do
          (aset out int-indx cur-phase)
          (gen-recur (rem (+ phase-incr cur-phase) 1.0)))
        (yield out))))

(defn phasor 
  "Phasor with frequency and starting phase"
  [freq phase]
  (if (and (number? freq) (number? phase))
    (phasor-fixed (double freq) (double phase)) 
    (vphasor freq phase)))

(defn sine 
  "Sine generator with fixed frequency and starting phase"
  ([^double freq]
   (sine freq 0.0))
  ([^double freq ^double phase]
   (let [phsr (phasor freq phase)
         out ^doubles (create-buffer)]
     (generator 
       [] [phs phsr]
       (do
         (aset out int-indx (Math/sin (* TWO_PI phs))) 
         (gen-recur))          
       (yield out)))))

;(require '[no.disassemble :refer :all])
;(println (disassemble sine))



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
           (aset out int-indx 0.0)
           (gen-recur))
         (let [v (Math/sin (* TWO_PI phase))]
           (aset out int-indx v)
           (gen-recur) ))
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
           (aset out int-indx 0.0)
           (gen-recur))
         (let [v (* amp (aget table (int (* phase tbl-len))))]
           (aset out int-indx v)
           (gen-recur)))
       (yield out)))))


(defn oscili
  "Linear-interpolating oscillator with table (defaults to sine wave table)"
  ([amp freq]
   (oscili amp freq sine-table 0))
  ([amp freq table]
   (oscili amp freq table 0))
  ([amp freq ^doubles table phase]
   (let [phsr (phasor (arg freq) phase)
         out ^doubles (create-buffer)
         tbl-len (alength table)
         ampfn (arg amp)]
     (generator 
       [] 
       [p phsr 
        amp ampfn]
       (if (= p Double/NEGATIVE_INFINITY) 
         (do 
           (aset out int-indx 0.0)
           (gen-recur))
         (let [phs (* p tbl-len)
               pt0 (int phs)
               pt1 (mod (inc pt0) tbl-len)  
               frac (if (zero? pt0) 
                      phs
                      (rem phs pt0))
               v0  (aget table pt0)
               v1  (aget table pt1)
               v (* amp (+ v0 (* frac (- v1 v0))))]
           (aset out int-indx v)
           (gen-recur)))
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
           (aset out int-indx 0.0)
           (gen-recur))
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
           (aset out int-indx v)
           (gen-recur)))
       (yield out)))))

;; Implementation of Bandlimited Impulse Train (BLIT) functions by Stilson and
;; Smith. 

(defn- calc-harmonics 
  ^long [^double p ^long nharmonics]
  (if (<= nharmonics 0)
    (let [max-harmonics (long (Math/floor (* 0.5 p)))]
      (+ (* 2 max-harmonics) 1))
    (+ (* 2 nharmonics) 1)))

(defn- pi-limit
  ^double [^double v]
  (if (>= v Math/PI) (- v Math/PI) v))

(def ^:const ^:private ^{:tag 'double} DOUBLE-EPSILON
  (Math/ulp 1.0))

;; blit-saw

(defn- blit
  ^double [^double phase ^double a ^double m ^double p]
  (let [denom (Math/sin phase)] 
    (if (<= (Math/abs denom) ^double DOUBLE-EPSILON)
      a
      (/ (Math/sin (* m phase)) (* p denom)))))

(defn- blit-saw-static
  [^double freq ^long nharmonics]
  (let [^doubles out (create-buffer)
        p (/ (double *sr*) freq)
        c2 (/ 1.0 p)
        rate (* Math/PI c2)
        m (calc-harmonics p nharmonics) 
        a (/ m p)]
    (generator
      [phase 0.0
       st (* -0.5 a)]
      [] 
      (let [tmp (+ (- st c2) (blit phase a m p))
            new-st (* tmp 0.995)
            new-phs (pi-limit (+ phase rate))]
        (aset out int-indx tmp) 
        (gen-recur new-phs new-st)) 
      (yield out))))

(defn- blit-saw-dynamic
  [freq ^long nharmonics]
  (let [out ^doubles (create-buffer)
          initialized (atom false)
          sr (double *sr*)]
    (generator
      [phase 0.0
       st 0.0]
      [f freq]
      (if (<= f 0.0)
        (do 
          (aset out int-indx 0.0)
          (gen-recur phase st))
        (let [p (/ sr f)
              c2 (/ 1.0 p)
              rate (* Math/PI c2)
              m (long (calc-harmonics p nharmonics))
              a (/ m p)
              st-val ^double (if @initialized 
                               st 
                               (reset! initialized (* -0.5 a)))
              tmp (+ (- ^double st-val c2) 
                     (blit phase a m p))
              new-st (* tmp 0.995)
              new-phs (pi-limit (+ phase rate))]
          (aset out int-indx tmp) 
          (gen-recur new-phs new-st)))
      (yield out))))

(defn blit-saw
  "Implementation of BLIT algorithm by Stilson and Smith for band-limited
  sawtooth waveform. Based on the C++ implementation from STK.
 
  Returns an optimized audio-function if freq is a number, or a slower
  version if freq is itself an audio-function."
  ([freq] (blit-saw freq 0))
  ([freq ^long nharmonics]
   {:pre [(or (and (number? freq) (pos? ^double freq)) (fn? freq))] }
  (if (number? freq)
    (blit-saw-static (double freq) nharmonics)    
    (blit-saw-dynamic freq nharmonics) 
    )))


;; BLIT Pulse

(defn- blit-pulse-static
  [^double freq ^double pulse-width ^long nharmonics]
  (let [out ^doubles (create-buffer)
        p (/ (double *sr*) freq)
        rate (/ Math/PI p)
        m (calc-harmonics p nharmonics)
        a (/ m p) ]
    (generator 
      [phase 0.0
       phase2 (* Math/PI pulse-width)
       last-val -0.5]
      []
      (let [denom (Math/sin phase)
            denom2 (Math/sin phase2)
            new-blit1 (blit phase a m p)
            new-blit2 (blit phase2 a m p) 
            new-blits (- new-blit1 new-blit2)
            new-val (+ new-blits last-val) 
            new-phs (pi-limit (+ phase rate))
            new-phs2 (pi-limit (+ phase2 rate))]
        (aset out int-indx new-val) 
        (gen-recur new-phs new-phs2 (* 0.999 new-val) ))
      (yield out))))

(defn- blit-pulse-dynamic
  [freq pulse-width ^long nharmonics]
  (let [out ^doubles (create-buffer)
        sr (double *sr*)]
    (generator
      [phase 0.0
       last-val -0.5]
      [f (arg freq)
       pw (arg pulse-width)]
      (if (<= f 0) 
        (do 
          (aset out int-indx 0.0)
          (gen-recur phase last-val))
        (let [p (/ sr f)
              rate (/ Math/PI p)
              m (calc-harmonics p nharmonics)
              a (/ m p) 
              phase2 (pi-limit (+ phase (* Math/PI pw)))
              denom (Math/sin phase)
              denom2 (Math/sin phase2)
              new-blit1 (blit phase a m p)
              new-blit2 (blit phase2 a m p) 
              new-blits (- new-blit1 new-blit2)
              new-val (+ new-blits last-val) 
              new-phs (pi-limit (+ phase rate)) ]
          (aset out int-indx new-val) 
          (gen-recur new-phs (* 0.999 new-val))))
      (yield out))))


(defn blit-pulse
  "Implementation of BLIT algorithm by Stilson and Smith for band-limited
  pulse waveform. 
 
  Returns an optimized audio-function if freq is a number, or a slower
  version if freq is itself an audio-function."
  ([freq pulse-width] (blit-pulse freq pulse-width 0))
  ([freq pulse-width ^long nharmonics]
   {:pre [(or (and (number? freq) (pos? ^double freq)) 
              (fn? freq)
              (and (number? pulse-width) (>= 0.0 ^double pulse-width 1.0)) 
              (fn? pulse-width))] }
  (if (and (number? freq) (number? pulse-width))
    (blit-pulse-static (double freq) (double pulse-width) nharmonics)    
    (blit-pulse-dynamic freq pulse-width nharmonics))))
 
(defn blit-square
  "Implementation of BLIT algorithm by Stilson and Smith for band-limited
  square waveform. 
 
  Returns an optimized audio-function if freq is a number, or a slower
  version if freq is itself an audio-function."
  ([freq] (blit-square freq 0))
  ([freq ^long nharmonics]
   {:pre [(or (and (number? freq) (pos? ^double freq)) (fn? freq))] }
  (if (number? freq)
    (blit-pulse-static (double freq) 0.5 nharmonics)    
    (blit-pulse-dynamic freq 0.5 nharmonics) 
    )))


;; BLIT Triangle
(defn- blit-triangle-static
  [^double freq ^long nharmonics]
  (let [out ^doubles (create-buffer)
        gain (/ (* 4.0 freq ) (double *sr*) )
        square (blit-square freq nharmonics)]
    (generator 
      [last-val -0.5]
      [square-val square]
      (let [new-val (+ (* 0.999 last-val) (* square-val gain))]
        (aset out int-indx new-val) 
        (gen-recur new-val))
      (yield out))))

(defn- blit-triangle-dynamic
  [freq ^long nharmonics]
  (let [out ^doubles (create-buffer)
        freq-fn (shared freq)
        sr (double *sr*)
        square (blit-square freq-fn nharmonics)]
    (generator 
      [last-val -0.5]
      [square-val square
       f freq-fn]
      (let [gain (/ (* 4.0 f ) sr )
            new-val (+ (* 0.999 last-val) (* square-val gain))]
        (aset out int-indx new-val) 
        (gen-recur new-val))
      (yield out))))

(defn blit-triangle
  "Generates BLIT-based triangle wave, via integration of BLIT square wave."
  ([freq] (blit-triangle freq 0))
  ([freq ^long nharmonics]
   {:pre [(or (and (number? freq) (pos? ^double freq)) (fn? freq))] }
  (if (number? freq)
    (blit-triangle-static (double freq) nharmonics)    
    (blit-triangle-dynamic freq nharmonics) 
    )))

;; LFO

(defn- lfo-ugen
  [freq ^IFn$DD phase-calc]
  (let [out (create-buffer)
        phsr (phasor freq 0.0)] 
     (generator
       []
       [phs phsr]
       (do
         (aset out int-indx ^double (.invokePrim phase-calc phs))
         (gen-recur)) 
       (yield out))))

(defn lfo 
  "Low-Frequency Oscillator with various types. Based on Csound's LFO opcode by
  John ffitch.  Accepted lfo-types are:
 
  TYPE              RANGE
  :sine             [-amp,amp]
  :triangle         [-amp,amp]
  :square           [-amp,amp]
  :square-unipolar  [0.0, amp] 
  :saw              [0.0, amp]
  :saw-down         [0.0, amp]
  
  Note: This is not a bandlimited oscillator and should be used only for
  parameter modulation. Also, unlike Csound's lfo, does not allow switching
  the lfo-type at performance time."
  ([amp freq]
   (lfo amp freq :sine))
  ([amp freq lfo-type]
   (case lfo-type
     :sine
     (oscil amp freq) 
     :triangle
     (mul amp
          (lfo-ugen freq
                    (fn ^double [^double p] 
                      (* 4.0 
                         (double
                           (cond 
                             (< p 0.25) p
                             (< p 0.5) (- 0.5 p)
                             (< p 0.75) (- (- p 0.5))
                             :else (- p 1.0) 
                             ))))))
     :square
     (mul amp (lfo-ugen freq (fn ^double [^double p] (if (< p 0.5) 1.0 -1.0))))
     :square-unipolar
     (mul amp (lfo-ugen freq (fn ^double [^double p] (if (< p 0.5) 1.0 0.0))))
     :saw
     (mul amp (phasor freq 0.0))
     :saw-down
     (mul amp (sub 1.0 (phasor freq 0.0))) 
     (throw (Exception. (str "Unknown LFO type: " lfo-type))) 
     )))


;; Impulse

(defn pulse 
  "Pulse generator. If freq is number and <= 0.0, will return a single
  impulse.  Otherwise returns impulses at given frequency. Freq may be 
  time-vary function."
  ([freq] (pulse freq 1.0))
  ([freq ^double amp]
  (if (and (number? freq) (<= ^double freq 0.0))
    (let [initial (let [b (create-buffer)]
                    (aset b 0 amp)
                    b)
          empty-buf (create-buffer)
          init-done (atom false)]
      (fn []
        (if @init-done 
          empty-buf 
          (do 
            (reset! init-done true)
            initial))))
    (let [out (create-buffer) 
          phsr (phasor freq 0.0)] 
      (generator
        [previous 100]
        [p phsr]
        (let [v (if (> previous p) amp 0.0)]
          (aset out int-indx v)
          (gen-recur p))
        (yield out))))))

;; UNIRECT 

(defn unirect 
  "Unipolar rectangle signal generator. Takes in frequency and duty cycle. Useful as a periodic gate signal."
  [freq duty-cycle]
  (let [f (arg freq) d (arg duty-cycle) sr (double *sr*)
        out (create-buffer)]
    (generator
      [t (long 0)]
      [_f f _d d]
      (let [dur (/ sr _f)
            duty-dur (* _d dur)
            t1 (inc t)
            new-t (if (>= t1 dur) 0 t1)]
        (aset out int-indx (if (< t duty-dur) 1.0 0.0)) 
        (gen-recur new-t))
      (yield out))))
