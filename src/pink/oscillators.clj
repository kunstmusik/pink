(ns pink.oscillators
  "Oscillator Functions"
  (:require [pink.config :refer [*sr*]]
            [pink.util :refer [create-buffer fill map-d 
                                     swapd! setd! getd arg]]
            [pink.gen :refer [gen-sine]] 
            ))

(def ^:const PI Math/PI)

(defmacro dec-if 
  [a] 
  `(if (>= ~a 1.0) (dec ~a) ~a))

(defn phasor 
  "Phasor with fixed frequency and starting phase"
  [^double freq ^double phase]
  (let [phase-incr ^double (/ freq  *sr*)
        cur-phase (double-array 1 phase)
        out (create-buffer)]
      (fn ^doubles [] 
        (fill out cur-phase #(dec-if (+ phase-incr ^double %))))))

(defn sine 
  "Sine generator with fixed frequency and starting phase"
  ([^double freq]
   (sine freq 0.0))
  ([^double freq ^double phase]
   (let [phsr (phasor freq phase)
         out (create-buffer)]
     (fn ^doubles []
       (map-d out #(Math/sin (* 2.0 PI ^double %)) (phsr))))))


(defmacro phs-incr
  [cur incr]
  `(dec-if (+ ~cur ~incr)))

(defn vphasor 
  "Phasor with variable frequency and fixed starting phase."
  [freq phase]
  {:pre (number? phase)}
  (let [out ^doubles (create-buffer)
        cur-phase (double-array 1 phase)
        len (alength ^doubles out)
        lastindx (dec len)
        ffn (arg freq)]
    (fn ^doubles [] 
      (let [f (ffn) ]
        (when f 
          (loop [i (unchecked-int 0)]
            (when (< i len)
              (let [incr ^double (/ (aget ^doubles f i) *sr*)] 
                (aset out i 
                      (setd! cur-phase (phs-incr (getd cur-phase) incr)))
                (recur (unchecked-inc-int i)))) 
            )
          out)))))

(defn sine2 
  "Sine generator with variable frequency and fixed starting phase."
  ([f]
   (sine2 f 0))
  ([f p]
   (let [phsr (vphasor (arg f) p)
         out (create-buffer)]
     (fn ^doubles []
       (map-d out #(Math/sin (* 2.0 PI ^double %)) (phsr))))))

(def sine-table (gen-sine))

;; fixme - handle amplitude as function by updating map-d to take in multiple buffers
(defn oscil
  "Oscillator with table (defaults to sine wave table, truncates indexing)"
  ([amp freq]
   (oscil amp freq sine-table 0))
  ([amp freq table]
   (oscil amp freq table 0))
  ([amp freq ^doubles table phase]
   (let [phsr (vphasor (arg freq) phase)
         out (create-buffer)
         tbl-len (alength table)
         ampfn (arg amp)]
      (fn ^doubles []
        (map-d out #(* %2 (aget table (int (* % tbl-len)))) (phsr) (ampfn))))))


(defn oscili
  "Linear-interpolating oscillator with table (defaults to sine wave table)"
  ([amp freq]
   (oscili amp freq sine-table 0))
  ([amp freq table]
   (oscili amp freq table 0))
  ([amp freq ^doubles table phase]
   (let [phsr (vphasor (arg freq) phase)
         out (create-buffer)
         tbl-len (alength table)
         ampfn (arg amp)]
      (fn ^doubles []
        (map-d out 
               #(let [phs (* % tbl-len)
                      pt0 (int phs)
                      pt1 (mod (inc pt0) tbl-len)  
                      frac (if (zero? pt0) 
                             phs
                             (rem phs pt0))
                      v0  (aget table pt0)
                      v1  (aget table pt1)]
                 (* %2 
                   (+ v0 (* frac (- v1 v0))))) 
               (phsr) (ampfn))))))


(defn oscil3
  "Cubic-interpolating oscillator with table (defaults to sine wave table) (based on Csound's oscil3)"
  ([amp freq]
   (oscil3 amp freq sine-table 0))
  ([amp freq table]
   (oscil3 amp freq table 0))
  ([amp freq ^doubles table phase]
   (let [phsr (vphasor (arg freq) phase)
         out (create-buffer)
         tbl-len (alength table)
         ampfn (arg amp)]
      (fn ^doubles []
        (map-d out 
               #(let [phs (* % tbl-len)
                      pt1 (int phs)
                      pt0 (if (zero? pt1) (- tbl-len 1) (- pt1 1))  
                      pt2 (mod (inc pt1) tbl-len)  
                      pt3 (mod (inc pt2) tbl-len)  
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
                      d p1 ]
                 (* %2 
                   (+ (* a x3) (* b x2) (* c x) d))) 
               (phsr) (ampfn))))))

