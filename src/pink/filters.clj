(ns pink.filters
  (:require [pink.config :refer [*sr* *buffer-size*]]
            [pink.util :refer :all]))

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
                (aset out indx new-val) 
                (recur (unchecked-inc-int indx) new-val))
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
      [last-val 0.0]
      [ain afn
       hp cutoff-fn]
      (let [ b (- 2.0 (Math/cos (* hp TPIDSR)))
                    c2 (- b (Math/sqrt (- (* b b) 1.0)))
                    new-val (* c2 (+ last-val ain))]
                (aset out indx new-val) 
                (recur (unchecked-inc-int indx) (- new-val ain)))
      (yield out))))

(defn port
  [afn ^double half-time] 
  "Apply portamento to step-wise signal via low-pass filtering."
  (let [out ^doubles (create-buffer) 
        onedsr (/ 1.0 (long *sr*))
        c2 (Math/pow 0.5 (/ onedsr half-time))
        c1 (- 1.0 c2)
        last-val ^doubles (double-array 1 0.0)]
    (generator
      [last-val 0.0]
      [ain afn]
       (let [new-val (+ (* c1 ain) (* c2 last-val))]
                (aset out indx new-val)
                (recur (unchecked-inc-int indx) new-val))
      (yield out))))

;; Butterworth Filters

(def ROOT2 (Math/sqrt 2))

(defmacro butter-filter
  [cut asig out indx a1 a2 a3 a4 a5 a6 a7]
  `(let [t# (- ~asig (* ~a4 ~a6) (* ~a5 ~a7))
        y# (+ (* t# ~a1) (* ~a2 ~a6) (* ~a3 ~a7))]
    (aset ~out ~indx y#) 
    (recur (unchecked-inc-int ~indx) ~cut ~a1 ~a2 ~a3 ~a4 ~a5 t# ~a6)))

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
          (aset out indx 0.0)
          (recur (unchecked-inc-int indx) cut old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (not= last-cut cut)
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
          (aset out indx 0.0)
          (recur (unchecked-inc-int indx) cut old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (not= last-cut cut)
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
    (recur (unchecked-inc-int ~indx) ~cf ~bw ~a1 ~a2 ~a3 ~a4 ~a5 t# ~a6)))

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
          (aset out indx 0.0)
          (recur (unchecked-inc-int indx) cf bw old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (or (not= last-cf cf) (not= last-bw bw))
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
          (aset out indx 0.0)
          (recur (unchecked-inc-int indx) cf bw old-a1 old-a2 old-a3 old-a4
                 old-a5 old-a6 old-a7))
        (or (not= last-cf cf) (not= last-bw bw))
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
(def ^:const TWO_PI (* 2.0 Math/PI))

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

;     (aset out indx _new-del5)
;     (recur _stg0 _stg1 _stg3 _stg3 _new-del4 _new-del5 
;            _new-tanhstg0 _new-tanhstg1 _new-tanhstg2
;            cut res acr tune))
;  )

(defn moogladder
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
     (if (or (not= old-freq cut) (not= old-res res))
       (let [fc (/ cut sr)
             f (* 0.5 fc)
             fc2 (* fc fc)
             fc3 (* fc2 fc)
             fcr (+ (* 1.8730 fc3) (* 0.4955 fc2)
                    (* -0.6490 fc) 0.9988)
             acr (+ (* -3.9364 fc2) (* 1.8409 fc) 0.9968)
             tune (/ (- 1.0 (Math/exp (- (* TWO_PI f fcr)))) THERMAL)
             res4 (* 4.0 res acr)]
         (let [
               input (- asig (* res4 del5))
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
               _stg1 (+ stg1 (* tune (- _new-tanhstg0 tanhstg1)))
               _new-tanhstg1 (Math/tanh (* _stg1 THERMAL))
               _stg2 (+ stg2 (* tune (- _new-tanhstg1 tanhstg2)))
               _new-tanhstg2 (Math/tanh (* _stg2 THERMAL))
               _stg3 (+ stg3 (* tune (- _new-tanhstg2 
                                        (Math/tanh (* stg3 THERMAL)))))
               _new-del5 (* 0.5 (+ _stg3 new-del4))
               _new-del4 _stg3]

           (aset out indx _new-del5)
           (recur (unchecked-inc-int indx) 
                  _stg0 _stg1 _stg3 _stg3 _new-del4 _new-del5 
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
               _stg1 (+ stg1 (* tune (- _new-tanhstg0 tanhstg1)))
               _new-tanhstg1 (Math/tanh (* _stg1 THERMAL))
               _stg2 (+ stg2 (* tune (- _new-tanhstg1 tanhstg2)))
               _new-tanhstg2 (Math/tanh (* _stg2 THERMAL))
               _stg3 (+ stg3 (* tune (- _new-tanhstg2 
                                        (Math/tanh (* stg3 THERMAL)))))
               _new-del5 (* 0.5 (+ _stg3 new-del4))
               _new-del4 _stg3]

           (aset out indx _new-del5)
           (recur (unchecked-inc-int indx) 
                  _stg0 _stg1 _stg3 _stg3 _new-del4 _new-del5 
                  _new-tanhstg0 _new-tanhstg1 _new-tanhstg2
                  cut res acr tune)))
       )
     
     (yield out)
     ) 
    ))
