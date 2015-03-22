(ns pink.filters
  (:require [pink.config :refer [*sr* *buffer-size*]]
            [pink.util :refer :all]
            [pink.delays :refer :all]
            [primitive-math :refer [not==]])
  (:import [clojure.lang IFn$DDDLO IFn$LD]))

(defn one-zero 
  "One-zero filter.

  z = location of zero along real axis in z-plane
  Difference equation: y(n) = x(n) - z * x(n-1)
  Reference: https://ccrma.stanford.edu/~jos/filters/One_Zero.html

  Based on code from Faust's filter.lib by Julius O. Smith"
  [afn z]
  (if (number? z)
    (let [out (create-buffer)
          coef (double z)] 
      (generator
        [xn_1 0.0] [sig afn]
        (let [v (- sig (* coef xn_1))] 
          (aset out int-indx v)
          (gen-recur sig))
        (yield out)))
    (let [out (create-buffer)] 
      (generator
        [xn_1 0.0] [sig afn, coef z]
        (let [v (- sig (* coef xn_1))] 
          (aset out int-indx v)
          (gen-recur sig))
        (yield out)))))

(defn one-pole
  "One-pole filter (aka \"leaky integrator\").

  p = pole location = feedback coefficient
  Difference equation: y(n) = x(n) + p * y(n-1)
  Reference: https://ccrma.stanford.edu/~jos/filters/One_Pole.html

  Based on code from Faust's filter.lib by Julius O. Smith"
  [afn p]
  (if (number? p)
    (let [out (create-buffer)
          coef (double p)] 
      (generator
        [yn_1 0.0] [sig afn]
        (let [v (+ sig (* coef yn_1))] 
          (aset out int-indx v)
          (gen-recur v))
        (yield out)))
    (let [out (create-buffer)] 
      (generator
        [yn_1 0.0] [sig afn, coef p]
        (let [v (+ sig (* coef yn_1))] 
          (aset out int-indx v)
          (gen-recur v))
        (yield out)))))

;; Comb Filters

(def ^:const LOG001 (Math/log 0.001))

(defn comb
  "Feedback Comb filter using fractional delay-line. reverb-time is time in
  seconds to decay 60db. loop-time is the length of the delay in seconds, which
  creates peaks at loop-time * sr/2, from 0 to sr/2 (Nyquist). loop-time is a
  static value, but reverb-time can be static or dynamic. 
  
  (NOTE: Only static reverb-time supported at the moment...)"
  [afn reverb-time ^double loop-time]
  (if (number? reverb-time)
    (let [out (create-buffer)
          reverb-time (double reverb-time)
          delay-time (* loop-time (double *sr*))
          delay-length (+ (long delay-time) 1)
          ^doubles delay-buffer (double-array delay-length)
          ^IFn$LD del-read (delay-readi delay-buffer delay-time)
          coef (double (Math/exp (* LOG001 (/ loop-time reverb-time))))]
      (generator
        [write-ptr (long 0)] [sig afn]
        (let [v (- sig (* coef (.invokePrim del-read write-ptr)))]
          (aset delay-buffer write-ptr v)
          (aset out int-indx v)
          (gen-recur (rem (unchecked-inc write-ptr) delay-length))) 
        (yield out)))))

;; 

(defn tone 
  "A first-order recursive low-pass filter with variable frequency response. (based on Csound's tone opcode). 
  
  For further information, see: http://csound.github.io/docs/manual/tone.html"
  [afn cutoff]
  (let [TPIDSR (/ (* 2 Math/PI) (long *sr*))
        cutoff-fn (arg cutoff)
        out ^doubles (create-buffer)]
    (generator 
      [last-val 0.0]
      [ain afn
       hp cutoff-fn]
      (let [b (- 2.0 (Math/cos (* hp TPIDSR)))
            c2 (- b (Math/sqrt (- (* b b) 1.0)))
            c1 (- 1.0 c2)
            new-val (+ (* c1 ain) (* c2 last-val))]
                (aset out int-indx new-val) 
                (gen-recur new-val))
      (yield out))))

;(println (disassemble tone))

(defn atone 
  "A hi-pass filter whose transfer functions are the complements of the tone function (based on Csound's atone opcode). 
  
  For further information, see: http://csound.github.io/docs/manual/atone.html"
  [afn cutoff]
  (let [TPIDSR (/ (* 2 Math/PI) (long *sr*))
        cutoff-fn (arg cutoff)
        out ^doubles (create-buffer)]
    (generator 
      [last-val 0.0] [ain afn, hp cutoff-fn]
      (let [ b (- 2.0 (Math/cos (* hp TPIDSR)))
                    c2 (- b (Math/sqrt (- (* b b) 1.0)))
                    new-val (* c2 (+ last-val ain))]
                (aset out int-indx new-val) 
                (gen-recur (- new-val ain)))
      (yield out))))

(defn port
  "Apply portamento to step-wise signal via low-pass filtering."
  [afn ^double half-time] 
  (let [out ^doubles (create-buffer) 
        onedsr (/ 1.0 (long *sr*))
        c2 (Math/pow 0.5 (/ onedsr half-time))
        c1 (- 1.0 c2)
        last-val ^doubles (double-array 1 0.0)]
    (generator
      [last-val 0.0] [ain afn]
       (let [new-val (+ (* c1 ain) (* c2 last-val))]
                (aset out int-indx new-val)
                (gen-recur new-val))
      (yield out))))

;; Butterworth Filters

(def ROOT2 (Math/sqrt 2))

(defmacro butter-filter
  [cut asig out indx a1 a2 a3 a4 a5 a6 a7]
  `(let [t# (- ~asig (* ~a4 ~a6) (* ~a5 ~a7))
        y# (+ (* t# ~a1) (* ~a2 ~a6) (* ~a3 ~a7))]
    (aset ~out ~indx y#) 
    (recur (unchecked-inc ~indx) ~cut ~a1 ~a2 ~a3 ~a4 ~a5 t# ~a6)))

(defn butterhp
  [afn cutoff]
  (let [out ^doubles (create-buffer)
        cut-fn (arg cutoff)
        PIDSR ^double (/ Math/PI (long *sr*))]
    (generator
      [last-cut 0.0
       old-a1 0.0
       old-a2 0.0
       old-a3 0.0
       old-a4 0.0
       old-a5 0.0
       old-a6 0.0
       old-a7 0.0]
      [asig afn
       cut cut-fn]
      (cond 
        (<= cut 0.0) 
        (do 
          (aset out int-indx 0.0)
          (gen-recur cut old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (not== last-cut cut)
        (let [c (Math/tan (* PIDSR cut)) 
              c2 (* c c)
              root2c (* (double ROOT2) c)
              a1 (/ 1.0 (+ 1.0 root2c c2))
              a2 (- (+ a1 a1)) 
              a3 a1
              a4 (* 2.0 (- c2 1.0) a1)
              a5 (* (+ (- 1.0 root2c) c2) a1)]
          (butter-filter cut asig out indx a1 a2 a3 a4 a5 old-a6 old-a7))
        :else 
        (butter-filter last-cut asig out indx old-a1 old-a2 old-a3 old-a4
                       old-a5 old-a6 old-a7))
      (yield out))))

(defn butterlp
  [afn cutoff]
  (let [out ^doubles (create-buffer)
        cut-fn (arg cutoff)
        PIDSR ^double (/ Math/PI (long *sr*))]
    (generator
      [last-cut 0.0
       old-a1 0.0
       old-a2 0.0
       old-a3 0.0
       old-a4 0.0
       old-a5 0.0
       old-a6 0.0
       old-a7 0.0]
      [asig afn
       cut cut-fn]
      (cond
        (<= cut 0.0) 
        (do 
          (aset out int-indx 0.0)
          (gen-recur cut old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (not== last-cut cut)
        (let [c (/ 1.0 (Math/tan (* PIDSR cut)))
              c2 (* c c)
              root2c (* (double ROOT2) c)
              a1 (/ 1.0 (+ 1.0 root2c c2))
              a2 (+ a1 a1) 
              a3 a1
              a4 (* 2.0 (- 1.0 c2) a1)
              a5 (* (+ (- 1.0 root2c) c2) a1)]
          (butter-filter cut asig out indx a1 a2 a3 a4 a5 old-a6 old-a7))

        :else 
        (butter-filter last-cut asig out indx old-a1 old-a2 old-a3 old-a4
                       old-a5 old-a6 old-a7))
      (yield out))))


(defmacro butterb-filter
  [cf bw asig out indx a1 a2 a3 a4 a5 a6 a7]
  `(let [t# (- ~asig (* ~a4 ~a6) (* ~a5 ~a7))
        y# (+ (* t# ~a1) (* ~a2 ~a6) (* ~a3 ~a7))]
    (aset ~out ~indx y#) 
    (recur (unchecked-inc ~indx) ~cf ~bw ~a1 ~a2 ~a3 ~a4 ~a5 t# ~a6)))

(defn butterbp
  [afn center-freq bandwidth ]
  (let [out ^doubles (create-buffer)
        cf-fn (arg center-freq)
        bw-fn (arg bandwidth)
        PIDSR ^double (/ Math/PI (long *sr*))
        TPIDSR ^double (/ (* 2.0 Math/PI) (long *sr*))]
    (generator
      [last-cf 0.0
       last-bw 0.0
       old-a1 0.0
       old-a2 0.0
       old-a3 0.0
       old-a4 0.0
       old-a5 0.0
       old-a6 0.0
       old-a7 0.0]
      [asig afn
       cf cf-fn
       bw bw-fn ]
      (cond 
        (or (<= cf 0.0) (<= bw 0.0))
        (do 
          (aset out int-indx 0.0)
          (gen-recur cf bw old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (or (not== last-cf cf) (not== last-bw bw))
        (let [c (/ 1.0 (Math/tan (* PIDSR bw)))
              d (* 2.0 (Math/cos (* TPIDSR cf)))
              a1 (/ 1.0 (+ 1.0 c))
              a3 (- a1)
              a4 (* (- c) d a1) 
              a5 (* (- c 1.0) a1)]
          (butterb-filter cf bw asig out indx a1 old-a2 a3 a4 a5 old-a6 old-a7))
        :else
        (butterb-filter last-cf last-bw asig out indx old-a1 old-a2 old-a3 old-a4
                       old-a5 old-a6 old-a7))
      (yield out))))

(defn butterbr
  [afn center-freq bandwidth ]
  (let [out ^doubles (create-buffer)
        cf-fn (arg center-freq)
        bw-fn (arg bandwidth)
        PIDSR ^double (/ Math/PI (long *sr*))
        TPIDSR ^double (/ (* 2.0 Math/PI) (long *sr*))]
    (generator
      [last-cf 0.0
       last-bw 0.0
       old-a1 0.0
       old-a2 0.0
       old-a3 0.0
       old-a4 0.0
       old-a5 0.0
       old-a6 0.0
       old-a7 0.0]
      [asig afn
       cf cf-fn
       bw bw-fn ]
      (cond
        (or (<= cf 0.0) (<= bw 0.0))
        (do 
          (aset out int-indx 0.0)
          (gen-recur cf bw old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (or (not== last-cf cf) (not== last-bw bw))
        (let [c (Math/tan (* PIDSR bw)) 
              d (* 2.0 (Math/cos (* TPIDSR cf)))
              a1 (/ 1.0 (+ 1.0 c))
              a2 (* (- d) a1) 
              a3 a1 
              a4 a2 
              a5 (* (- 1.0 c) a1)]
          (butterb-filter cf bw asig out indx a1 a2 a3 a4 a5 old-a6 old-a7))
        :else
        (butterb-filter last-cf last-bw asig out indx old-a1 old-a2 old-a3 old-a4
                       old-a5 old-a6 old-a7))
      (yield out))))

;; Moog Ladder Filter

; transistor thermal voltage
(def ^:const THERMAL (/ 1.0 40000.0))
(def ^:private ^:const TWO_PI (* 2.0 Math/PI))

;(defn ladder-section)

;(defmacro moogladder-process
;  [asig cut res acr tune]
;  `(let [res4 (* 4.0 res acr)]
;         input# (- ~asig (* res4# del5#))
;         stg0 (+ del0 (* tune (- (Math/tanh (* input THERMAL)) 
;                                 tanhstg0)))
;         new-tanhstg0 (Math/tanh (* stg0 THERMAL))
;         stg1 (+ del1 (* tune (- new-tanhstg0 tanhstg1)))
;         new-tanhstg1 (Math/tanh (* stg1 THERMAL))
;         stg2 (+ del2 (* tune (- new-tanhstg1 tanhstg2)))
;         new-tanhstg2 (Math/tanh (* stg2 THERMAL))
;         stg3 (+ del3 (* tune (- new-tanhstg2 
;                                 (Math/tanh (* del3 THERMAL)))))
;         new-del5 (* 0.5 (+ stg3 del4))
;         new-del4 stg3

;         ;; second pass of 2x oversampling
;         _input (- asig (* res4 new-del5))
;         _stg0 (+ stg0 (* tune (- (Math/tanh (* _input THERMAL)) 
;                                  new-tanhstg0)))
;         _new-tanhstg0 (Math/tanh (* _stg0 THERMAL))
;         _stg1 (+ stg1 (* tune (- _new-tanhstg0 tanhstg1)))
;         _new-tanhstg1 (Math/tanh (* _stg1 THERMAL))
;         _stg2 (+ stg2 (* tune (- _new-tanhstg1 tanhstg2)))
;         _new-tanhstg2 (Math/tanh (* _stg2 THERMAL))
;         _stg3 (+ stg3 (* tune (- _new-tanhstg2 
;                                  (Math/tanh (* stg3 THERMAL)))))
;         _new-del5 (* 0.5 (+ _stg3 _del4))
;         _new-del4 _stg3]

;     (aset out int-indx _new-del5)
;     (recur _stg0 _stg1 _stg3 _stg3 _new-del4 _new-del5 
;            _new-tanhstg0 _new-tanhstg1 _new-tanhstg2
;            cut res acr tune))
;  )

(defn moogladder
  "Moogladder is a digital implementation of the Moog ladder filter based on
  the work of Antti Huovilainen, described in the paper \"Non-Linear Digital
  Implementation of the Moog Ladder Filter\" (Proceedings of DaFX04, Univ of
  Napoli). This implementation is probably a more accurate digital
  representation of the original analogue filter.  
  
  Translation of Csound moogladder opcode, written by Victor Lazzarini.
 
  http://csound.github.io/docs/manual/moogladder.html"
  [afn cutoff resonance]
  (let [out ^doubles (create-buffer)
        cfn (arg cutoff)
        rfn (arg resonance)
        sr (long *sr*)]
   (generator
     [del0 0.0 del1 0.0 del2 0.0 del3 0.0 del4 0.0 del5 0.0
      tanhstg0 0.0 tanhstg1 0.0 tanhstg2 0.0
      old-freq 0.0
      old-res -1.0
      old-acr 0.0
      old-tune 0.0]
     [asig afn
      cut cfn
      res rfn]
     (if (or (not== old-freq cut) (not== old-res res))
       (let [fc (/ cut sr)
             f (* 0.5 fc)
             fc2 (* fc fc)
             fc3 (* fc2 fc)
             fcr (+ (* 1.8730 fc3) (* 0.4955 fc2)
                    (* -0.6490 fc) 0.9988)
             acr (+ (* -3.9364 fc2) (* 1.8409 fc) 0.9968)
             tune (/ (- 1.0 (Math/exp (- (* TWO_PI f fcr)))) THERMAL)
             res4 (* 4.0 res acr)]
         (let [input (- asig (* res4 del5))
               stg0 (+ del0 (* tune (- (Math/tanh (* input THERMAL)) 
                                       tanhstg0)))
               new-tanhstg0 (Math/tanh (* stg0 THERMAL))
               stg1 (+ del1 (* tune (- new-tanhstg0 tanhstg1)))
               new-tanhstg1 (Math/tanh (* stg1 THERMAL))
               stg2 (+ del2 (* tune (- new-tanhstg1 tanhstg2)))
               new-tanhstg2 (Math/tanh (* stg2 THERMAL))
               stg3 (+ del3 (* tune (- new-tanhstg2 
                                       (Math/tanh (* del3 THERMAL)))))
               new-del5 (* 0.5 (+ stg3 del4))
               new-del4 stg3

               ;; second pass of 2x oversampling
               _input (- asig (* res4 new-del5))
               _stg0 (+ stg0 (* tune (- (Math/tanh (* _input THERMAL)) 
                                        new-tanhstg0)))
               _new-tanhstg0 (Math/tanh (* _stg0 THERMAL))
               _stg1 (+ stg1 (* tune (- _new-tanhstg0 new-tanhstg1)))
               _new-tanhstg1 (Math/tanh (* _stg1 THERMAL))
               _stg2 (+ stg2 (* tune (- _new-tanhstg1 new-tanhstg2)))
               _new-tanhstg2 (Math/tanh (* _stg2 THERMAL))
               _stg3 (+ stg3 (* tune (- _new-tanhstg2 
                                        (Math/tanh (* stg3 THERMAL)))))
               _new-del5 (* 0.5 (+ _stg3 new-del4))
               _new-del4 _stg3]

           (aset out int-indx _new-del5)
           (gen-recur 
                  _stg0 _stg1 _stg2 _stg3 _new-del4 _new-del5 
                  _new-tanhstg0 _new-tanhstg1 _new-tanhstg2
                  cut res acr tune))) 
       (let [acr old-acr 
             tune old-tune 
             res4 (* 4.0 res acr)]
         (let [input (- asig (* res4 del5))
               stg0 (+ del0 (* tune (- (Math/tanh (* input THERMAL)) 
                                       tanhstg0)))
               new-tanhstg0 (Math/tanh (* stg0 THERMAL))
               stg1 (+ del1 (* tune (- new-tanhstg0 tanhstg1)))
               new-tanhstg1 (Math/tanh (* stg1 THERMAL))
               stg2 (+ del2 (* tune (- new-tanhstg1 tanhstg2)))
               new-tanhstg2 (Math/tanh (* stg2 THERMAL))
               stg3 (+ del3 (* tune (- new-tanhstg2 
                                       (Math/tanh (* del3 THERMAL)))))
               new-del5 (* 0.5 (+ stg3 del4))
               new-del4 stg3

               ;; second pass of 2x oversampling
               _input (- asig (* res4 new-del5))
               _stg0 (+ stg0 (* tune (- (Math/tanh (* _input THERMAL)) 
                                        new-tanhstg0)))
               _new-tanhstg0 (Math/tanh (* _stg0 THERMAL))
               _stg1 (+ stg1 (* tune (- _new-tanhstg0 new-tanhstg1)))
               _new-tanhstg1 (Math/tanh (* _stg1 THERMAL))
               _stg2 (+ stg2 (* tune (- _new-tanhstg1 new-tanhstg2)))
               _new-tanhstg2 (Math/tanh (* _stg2 THERMAL))
               _stg3 (+ stg3 (* tune (- _new-tanhstg2 
                                        (Math/tanh (* stg3 THERMAL)))))
               _new-del5 (* 0.5 (+ _stg3 new-del4))
               _new-del4 _stg3]

           (aset out int-indx _new-del5)
           (gen-recur 
                  _stg0 _stg1 _stg2 _stg3 _new-del4 _new-del5 
                  _new-tanhstg0 _new-tanhstg1 _new-tanhstg2
                  cut res acr tune)))
       )
     
     (yield out)
     ) 
    ))

;; TODO - verify lpf18 output matches that of Csound's
;; TODO - Optimize code below to check if values have changed, if not, 
;;        don't recompute coefficients (same what Csound does)
(defn lpf18
  "Josep Comajuncosas' 18dB/oct resonant 3-pole LPF with tanh dist.
  Coded in C by John ffitch, 2000 Dec 17, for Csound 
  Translated to Clojure by Steven Yi.
 
  http://csound.github.io/docs/manual/lpf18.html"
  [afn cutoff resonance distortion]
  (let [cutfn (arg cutoff)
        resfn (arg resonance)
        distfn (arg distortion)
        sr (long *sr*)
        onedsr (/ 1.0 (double *sr*))
        out (create-buffer)] 
    (generator
      [lastin 0.0
       lastres 0.0
       lastcut 0.0
       lastdist 0.0
       ay1 0.0
       ay2 0.0
       aout 0.0]
      [ain afn
       cut cutfn
       res resfn
       dist distfn]
      (let [kfcn (* 2.0 cut onedsr)
            kp (+ (* (+ (* -2.7528 kfcn) 3.0429) 
                     kfcn)
                  (* 1.718 kfcn)
                  -0.9984)
            kp1 (+ 1.0 kp)
            kp1h (* 0.5 kp1)
            kres (* res 
                    (+ (* -2.7079 kp1) (* 10.963 kp1)
                       (* -14.934 kp1) 8.4974))
            value (+ 1.0 (* dist (+ 1.5 
                                    (* 2.0 kres (- 1.0 kfcn)))))

            curin (- ain (Math/tanh (* kres aout)))
            curay1 (- (* kp1h (+ lastin lastin)) (* kp ay1))
            curay2 (- (* kp1h (+ curay1 ay1)) (* kp ay2))
            curaout (- (* kp1h (+ curay2 ay2)) (* kp aout))
            outval (Math/tanh (* curaout value))] 
        (aset out int-indx outval)
        (gen-recur curin res cut dist curay1 curay2 curaout))
      (yield out))))

;; State Variable Filter

(defmacro ^:private write-filt-state
  [filt-state & values]
  `(do ~@(map (fn [a b] `(aset ~filt-state ~a ~b))
       (range (count values)) values)))

(defn- proc-statevar-fn
  [^doubles filt-state]
  (fn ^doubles 
    [^double ain ^double freq ^double q ^long oversampling]
    (loop [indx 0
           hp (aget filt-state 0) 
           lp (aget filt-state 1) 
           bp (aget filt-state 2)
           br (aget filt-state 3)]
      (if (< indx oversampling)
        (let [new-hp (- ain (* q bp) lp) 
              new-bp (+ (* new-hp freq) bp)     
              new-lp (+ (* bp freq) lp)
              new-br (+ new-lp new-hp)]
          (recur (unchecked-inc indx) new-hp new-lp new-bp new-br)) 
        (do 
          (write-filt-state filt-state hp lp bp br)
          filt-state)))))

(defn statevar 
  "Statevar is a digital implementation of the analogue state-variable
  filter. This filter has four simultaneous outputs: high-pass, low-pass,
  band-pass and band-reject. This filter uses oversampling for sharper
  resonance (default: 3 times oversampling). It includes a resonance limiter
  that prevents the filter from getting unstable.
  
  Translation of Csound statevar opcode, written by Victor Lazzarini.
 
  http://csound.github.io/docs/manual/statevar.html"
  ([afn freq res]
   (statevar afn freq res 3))
  ([afn freq res ^long oversampling]
   (let [hp-out (create-buffer)
         lp-out (create-buffer)
         bp-out (create-buffer)
         br-out (create-buffer)
         out (into-array [hp-out lp-out bp-out br-out])
         freqfn (arg freq)
         resfn (arg res)
         mul-val (double (/ (/ Math/PI (double *sr*)) oversampling))
         filt-state (double-array 4)
         process ^IFn$DDDLO (proc-statevar-fn filt-state)]
     (generator
       [] 
       [sig afn
        f freqfn
        q resfn]
       (let [fval (* 2.0 (Math/sin (* f mul-val)))
             qval (/ 1.0 q)
             lim (/ (* (- 2.0 fval) 0.05) oversampling)
             qval (if (< qval lim) lim qval)]
         (process sig fval qval oversampling)
         (aset hp-out int-indx (aget filt-state 0))  
         (aset lp-out int-indx (aget filt-state 1))  
         (aset bp-out int-indx (aget filt-state 2))  
         (aset br-out int-indx (aget filt-state 3))  
         (gen-recur)) 
       (yield out)))))

;; General Filters

;; Biquad

;(defn biquad 
;  [afn b0 b1 b2 a1 a2]
;  (let [^doubles out (create-buffer)
;        _a1 (double a1) 
;        _a2 (double a2) 
;        _b0 (double b0) 
;        _b1 (double b1) 
;        _b2 (double b2) 
;        ]
;   (generator
;     [x-1 0.0 x-2 0.0 y-1 0.0 y-2 0.0]
;     [xn afn]
;     (let [yn (- (+ (* _b0 xn) (* b1 x-1) (* b2 x-2))
;                 (* _a1 y-1) (* _a2 y-2))]
;       (aset out int-indx yn)
;       (gen-recur xn x-1 yn y-1))
;     (yield out)
;     )))



