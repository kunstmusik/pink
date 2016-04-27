(ns pink.instruments.piano
  "Translation of Scott Van Duyne's Piano Model from Common Lisp Music"
  (:require [pink.util :refer :all]
            [pink.config :refer :all]
            [pink.delays :refer [delay-read adelay]]
             [pink.noise :refer [white-noise]])
  (:import [clojure.lang IFn$LD IFn$DD]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def number-of-stiffness-allpasses 8)
(def longitudinal-mode-cutoff-keynum 29)
(def longitudinal-mode-stiffness-coefficient -0.5)
(def ^:const golden-mean 0.618)
(def loop-gain-env-t60 0.05)
(def loop-gain-default 0.9999)
(def nstrings 3)
(def ^:const TWO-PI ^double (* 2.0 Math/PI))

;;keynum indexed parameter tables
;;these should all be &key variable defaults for p instrument

(def default-loudpole-table
  '(36 0.8 60 0.85 84 0.7 96 0.6 108 0.5))

(def default-softpole-table
  '(36 0.93 60 0.9 84 0.9 96 0.8 108 0.8))

(def default-loudgain-table
  '(21.000 0.700 36.000 0.700 48.000 0.700 60.000 0.650 72.000 0.650 84.000 0.650 87.006 0.681 88.070 0.444 90.653 0.606 95.515 0.731 99.770 0.775 101.897 0.794 104.024 0.800 105.695 0.806))

(def default-softgain-table
  '(21 0.25 108 0.25))

(def default-strikeposition-table
  '(21.000 0.140 23.884 0.139 36.000 0.128 56.756 0.129 57.765 0.130 59.000 0.130 60.000 0.128 61.000 0.128 62.000 0.129 66.128 0.129 69.000 0.128 72.000 0.128 73.000 0.128 79.000 0.128 80.000 0.128 96.000 0.128 99.000 0.128))

(def default-detuning2-table
  '(22.017 -0.090 23.744 -0.090 36.000 -0.080 48.055 -0.113 60.000 -0.135 67.264 -0.160 72.000 -0.200 84.054 -0.301 96.148 -0.383 108 -0.383))

(def default-detuning3-table '(21.435 0.027 23.317 0.043 36.000 0.030 48.000 0.030 60.000 0.030 72.000 0.020 83.984 0.034 96.000 0.034 99.766 0.034))

(def default-stiffnesscoefficient-table
  '(21.000 -0.920 24.000 -0.900 36.000 -0.700 48.000 -0.250 60.000 -0.100 75.179 -0.040 82.986 -0.040 92.240 -0.040 96.000 -0.040 99.000 .2 108.000 0.5))

(def default-singlestringdecayrate-table
  '(21.678 -2.895 24.000 -3.000 36.000 -4.641 41.953 -5.867 48.173 -7.113 53.818 -8.016 59.693 -8.875 66.605 -9.434 73.056 -10.035 78.931 -10.293 84.000 -12.185))

(def default-singlestringzero-table
  '(21.000 -0.300 24.466 -0.117 28.763 -0.047 36.000 -0.030 48.000 -0.020 60.000 -0.010 72.000 -0.010 84.000 -0.010 96.000 -0.010))

(def default-singlestringpole-table
  '(21.000 0 24.466 0 28.763 0 36.000 0 108 0))

(def default-releaseloopgain-table
  '(21.643 0.739 24.000 0.800 36.000 0.880 48.000 0.910 60.000 0.940 72.000 0.965 84.000 0.987 88.99 0.987 89.0 1.0 108 1.0))

(def default-drytapfiltcoeft60-table
  '(36 0.35 60 0.25 108 0.15))

(def default-drytapfiltcoeftarget-table
  '(36 -0.8 60 -0.5 84 -0.4 108 -0.1))

(def default-drytapfiltcoefcurrent-table
  '(0 0 200 0))

(def default-drytapampt60-table
  '(36 0.55 60 0.5 108 0.45))

(def default-sustainpedallevel-table
  '(21.000 0.250 24.000 0.250 36.000 0.200 48.000 0.125 60.000 0.075 72.000 0.050 84.000 0.030 96.000 0.010 99.000 0.010))

(def default-pedalresonancepole-table
  '(20.841 0.534 21.794 0.518 33.222 0.386 45.127 0.148 55.445 -0.065 69.255 -0.409 82.905 -0.729 95.763 -0.869 106.398 -0.861))

(def default-pedalenvelopet60-table
  '(21.0 7.5 108.0 7.5))

(def default-soundboardcutofft60-table
  '(21.0 0.25 108.0 0.25))

(def default-drypedalresonancefactor-table
  '(21.0 0.5 108.0 0.5))

(def default-unacordagain-table
  '(21 1.0 24 0.4 29 0.1 29.1 0.95 108 0.95))


;; converts t60 values to suitable :rate values for expseg
(defn- in-t60 
  ^double [^double t60] 
  (- 1.0 (Math/pow 0.001 (/ 1.0 t60 (double *sr*)))))

(defn- expseg
  ([^double cur-val ^double target-val rate]
   (expseg cur-val target-val rate :sustain))
  ([^double cur-val ^double target-val rate done-action]
   (if (fn? rate)
     (let [out (create-buffer)
           sustain-buffer (create-buffer target-val)
           ;; assumes downwards expseg
           bound (* target-val 1.001)]
       (generator
         [v cur-val]
         [r rate]
         (if (and (= int-indx 0) (= v target-val))
           (if (= done-action :sustain)
             sustain-buffer
             nil)
           (let [new-v (if (< v bound)
                         target-val 
                         (+ v (* (- target-val v) r))) ]
             (aset out int-indx new-v)
             (gen-recur new-v)))
         (yield out)))
     (let [out (create-buffer)
           sustain-buffer (create-buffer target-val)
           ;; assumes downwards expseg
           bound (* target-val 1.001)
           r (double rate)]
       (generator
         [v cur-val]
         []
         (if (and (= int-indx 0) (= v target-val))
           (if (= done-action :sustain)
             sustain-buffer
             nil)
           (let [new-v (if (< v bound)
                         target-val 
                         (+ v (* (- target-val v) r))) ]
             (aset out int-indx new-v)
             (gen-recur new-v)))
         (yield out))))))


;; based on one-pole in CLM's mus.lisp
(defn- one-pole 
  [afn ^double a0 ^double b1]
  (let [out (create-buffer)]
    (generator
      [y1 0.0]
      [sig afn]
      (let [v (- (* a0 sig) (* b1 y1))]
        (aset out int-indx v) 
        (gen-recur v))
      (yield out))))

(defn- one-pole-swept
  [afn coef]
  (let [out (create-buffer)
        c (arg coef)
        ]
    (generator
      [y1 0.0]
      [sig afn 
       _coef c]
      (let [v (- (* (+ 1.0 _coef) sig) (* _coef y1))]
        (aset out int-indx v)
        (gen-recur v))
      (yield out))))

(defn- one-pole-allpass
  [afn ^double coef] 
  (let [out (create-buffer)]
    (generator
      [x1 0.0 y1 0.0]
      [sig afn]
      (let [v (+ (* coef (- sig y1)) x1)]
        (aset out int-indx v)
        (gen-recur sig v))
      (yield out))))

(defn- one-pole-one-zero
  "y(n) = a0 (xn) + a1 x(n-1) - b0 y(n-1)"
  [afn ^double a0 ^double a1 ^double b1]
  (let [out (create-buffer)]
    (generator
      [x1 0.0 y1 0.0] 
      [sig afn]
      (let [v (- (+ (* a0 sig) (* a1 x1)) 
                 (* b1 y1))]
        (aset out int-indx v) 
        (gen-recur sig v))
      (yield out))))

;; Not using 'very special noise generator' from piano.clm as it
;; used ratio math which would be very slow...
(defn- noise
  [^double amp]
  (mul amp (white-noise)))

(defn- apphase ^double [^double a1 ^double wt]
  (Math/atan2 (* (- (* a1 a1) 1.0) (Math/sin wt))
        (+ (* 2.0 a1) (*(+ (* a1 a1) 1.0) (Math/cos wt)))))

(defn- opozphase ^double [^double b0 ^double b1 ^double a1 ^double wt] 
  (let [s (Math/sin wt)
        c (Math/cos wt)] 
    (Math/atan2 (- (* a1 s (+ b0 (* b1 c))) (* b1 s (+ 1.0 (* a1 c))))
          (+ (* (+ b0 (* b1 c)) (+ 1.0 (* a1 c))) (* b1 s a1 s)))))

(defn- get-allpass-coef ^double [^double samp-frac ^double wt]
  (let [ta (Math/tan (- (* samp-frac wt)))
        c (Math/cos wt)
        s (Math/sin wt)]
    (/ (+ (- ta) (* (Math/signum ta)
                    (Math/sqrt (* (+ 1 (* ta ta)) (* s s)))))
       (- (* c ta) s))))

(defn- apfloor [^double len ^double wt]
  (let [len-int (atom (Math/floor len))
        len-frac (atom (- len ^double @len-int))]
    (when (< ^double @len-frac golden-mean)
      (swap! len-int dec)
      (swap! len-frac inc))
    (when (and (< ^double @len-frac golden-mean)
               (> ^double @len-int 0))
      (swap! len-int dec)
      (swap! len-frac inc))
    [@len-int (get-allpass-coef @len-frac wt)]))

(defn- tune-piano
  [frequency stiffnesscoefficient numallpasses b0 b1 a1] 
  (let [frequency (double frequency) 
        stiffnesscoefficient (double stiffnesscoefficient) 
        numallpasses (double numallpasses) 
        b0 (double b0) 
        b1 (double b1) 
        a1 (double a1)
        wt (/ (* frequency TWO-PI) (double *sr*))
        len (/ (+ TWO-PI 
                (* numallpasses
                   (apphase stiffnesscoefficient wt))
                (opozphase (+ 1 (* 3 b0)) (+ a1 (* 3 b1)) a1 wt))
             wt)]
    (apfloor len wt)))

(defn- envelope-interp
  ^double [^double x table]
  (let [[^double a ^double b & r] table]
    (when (and a b) 
      (if (<= x a) 
        b
        (let [[^double c ^double d] r] 
          (cond 
            (nil? c)
            b
            (and (>= x a) (< x c))
            (+ b (* (/ (- x a) (- c a)) (- d b)))  
            :else
            (recur x r)))))))


(defn- arg-lookup 
  ^double [args kwd keynum table]
  (if-let [v (kwd args)] 
    (double v)
    (envelope-interp keynum table)))

(defn- make-ss-delay 
  "Single-sample frac delay"
  ^IFn$DD [^double delay-time]
  (if (= 0.0 delay-time)
    (fn ^double [^double input] input)
    (let [del-time-samps (long delay-time) 
          delay-buffer (double-array del-time-samps)
          rw-ptr (long-array 1 0)
          ^IFn$LD del-read (delay-read delay-buffer del-time-samps)]
      (fn ^double [^double input]
        (let [index (long (aget rw-ptr 0)) 
              v (.invokePrim del-read index)]
          (aset delay-buffer index input)
          (aset rw-ptr 0 (rem (inc index) del-time-samps))
          v)))))

(defn- ss-one-pole-allpass 
  "Single-sample one-pole allpass"
  ^IFn$DD [^double coef]
  (let [x1 (double-array 1 0)
        y1 (double-array 1 0)]
    (fn ^double [^double input]
      (let [_x1 (aget x1 0)
            _y1 (aget y1 0)
            v (+ (* coef (- input _y1)) _x1)]
        (aset x1 0 input) 
        (aset y1 0 v) 
        v))))


(defn- ss-one-pole-one-zero
  "y(n) = a0 (xn) + a1 x(n-1) - b0 y(n-1)"
  ^IFn$DD [^double a0 ^double a1 ^double b1]
  (let [x1 (double-array 1 0.0) y1 (double-array 1 0.0)] 
    (fn ^double [^double input]
      (let [v (- (+ (* a0 input) (* a1 (aget x1 0))) 
                 (* b1 (aget y1 0)))]
        (aset x1 0 input)
        (aset y1 0 v)
        v))))

(defn- make-string-delay
  ^IFn$DD [^double delay-len ^double tuning-coef ^double stiffness-coef]
  (let [^IFn$DD d (make-ss-delay (- delay-len 1.0))
        ^IFn$DD t (ss-one-pole-allpass tuning-coef)
        ^IFn$DD a1 (ss-one-pole-allpass stiffness-coef)
        ^IFn$DD a2 (ss-one-pole-allpass stiffness-coef)
        ^IFn$DD a3 (ss-one-pole-allpass stiffness-coef)
        ^IFn$DD a4 (ss-one-pole-allpass stiffness-coef)
        ^IFn$DD a5 (ss-one-pole-allpass stiffness-coef)
        ^IFn$DD a6 (ss-one-pole-allpass stiffness-coef)
        ^IFn$DD a7 (ss-one-pole-allpass stiffness-coef)
        ^IFn$DD a8 (ss-one-pole-allpass stiffness-coef)]
    (fn ^double [^double input]
      (->> (.invokePrim a8 input)
          (.invokePrim a7)
          (.invokePrim a6)
          (.invokePrim a5)
          (.invokePrim a4)
          (.invokePrim a3)
          (.invokePrim a2)
          (.invokePrim a1)
          (.invokePrim t)
          (.invokePrim d)))))


(defn piano 
  "Physically-modelled piano instrument. Based on Scott Van Duyne's
  Piano Model from Common Lisp Music. Arguments are given as key/values."
  [& {:keys 
      [ ^double duration ^double keynum strike-velocity 
       pedal-down release-time-margin amp 
       detuningfactor detuningfactor-table  ^double stiffnessfactor 
       stiffnessfactor-table pedalpresencefactor longitudinalmode 
       strikepositioninvfac singlestringdecayratefactor]
      :or {
           duration 1.0
           keynum 60.0                     ; middleC=60: can use fractional part to detune 
           strike-velocity 0.5            ; corresponding normalized velocities (range: 0.0--1.0)
           pedal-down false                 ; set to #t for sustain pedal down...pedal-down-times not yet implemented
           release-time-margin 0.75      ; extra compute time allowed beyond duration
           amp 0.5                        ; amp scale of noise inputs...

           ;;slider controls
           detuningfactor 1.0
           detuningfactor-table  nil
           stiffnessfactor 1.0
           stiffnessfactor-table nil
           pedalpresencefactor 0.3
           longitudinalmode 10.5
           strikepositioninvfac -0.9
           singlestringdecayratefactor 1.0 }
      :as args}]

  (let [
        dur (long (Math/floor (* (double duration) (double *sr*))))

        freq (* 440.0 (Math/pow 2.0 (/ (- (double keynum) 69.0) 12.0)))
        wt (/ (* 2.0 Math/PI (double freq)) (double *sr*)) 

        loudpole (arg-lookup args :loudpole keynum default-loudpole-table)
        softpole (arg-lookup args :softpole keynum default-softpole-table)
        loudgain (arg-lookup args :loudgain keynum default-loudgain-table)
        softgain (arg-lookup args :softgain keynum default-softgain-table)
        strikeposition (arg-lookup args :strikeposition keynum default-strikeposition-table)
        detuning2 (arg-lookup args :detuning2 keynum default-detuning2-table)
        detuning3 (arg-lookup args :detuning3 keynum default-detuning3-table)
        stiffnesscoefficient (arg-lookup args :stiffnesscoefficient keynum default-stiffnesscoefficient-table)
        singlestringdecayrate (arg-lookup args :singlestringdecayrate keynum default-singlestringdecayrate-table)
        singlestringzero (arg-lookup args :singlestringzero keynum default-singlestringzero-table)
        singlestringpole (arg-lookup args :singlestringpole keynum default-singlestringpole-table)
        releaseloopgain (arg-lookup args :releaseloopgain keynum default-releaseloopgain-table)
        drytapfiltcoeft60 (arg-lookup args :drytapfiltcoeft60 keynum default-drytapfiltcoeft60-table)
        drytapfiltcoeftarget (arg-lookup args :drytapfiltcoeftarget keynum default-drytapfiltcoeftarget-table)
        drytapfiltcoefcurrent (arg-lookup args :drytapfiltcoefcurrent keynum default-drytapfiltcoefcurrent-table)
        drytapampt60 (arg-lookup args :drytapampt60 keynum default-drytapampt60-table)
        sustainpedallevel (arg-lookup args :sustainpedallevel keynum default-sustainpedallevel-table)
        pedalresonancepole (arg-lookup args :pedalresonancepole keynum default-pedalresonancepole-table)
        pedalenvelopet60 (arg-lookup args :pedalenvelopet60 keynum default-pedalenvelopet60-table)
        soundboardcutofft60 (arg-lookup args :soundboardcutofft60 keynum default-soundboardcutofft60-table)
        drypedalresonancefactor (arg-lookup args :drypedalresonancefactor keynum default-drypedalresonancefactor-table)
        unacordagain (arg-lookup args :unacordagain keynum default-unacordagain-table)


        sb-cutoff-rate (in-t60 soundboardcutofft60)

        ;; generators


        dry-tap (mul (expseg 1.0 0.0 
                             (hold-until duration 
                                         (in-t60 drytapampt60)
                                         sb-cutoff-rate))
                     (one-pole-swept 
                       (one-pole-one-zero (noise amp) 1.0 0.0 0.0) 
                       (expseg drytapfiltcoefcurrent 
                               drytapfiltcoeftarget
                               drytapfiltcoeft60)))

        open-strings
        (mul (expseg (* sustainpedallevel (double pedalpresencefactor)
                        (if pedal-down 1.0 drypedalresonancefactor))
                     0.0
                     (hold-until duration 
                                 (in-t60 pedalenvelopet60)
                                 sb-cutoff-rate))
             (one-pole-swept 
               (one-pole-one-zero (noise amp) 1.0 0.0 0.0) 
               (expseg 0.0 -0.5 (in-t60 pedalenvelopet60))))

        total-tap (sum dry-tap open-strings)



        hammerpole
        (+ softpole (* (- loudpole softpole) (double strike-velocity))) 
        hammergain
        (+ softgain (* (- loudgain softgain) (double strike-velocity)))

        adelin 
        (shared
          (-> total-tap 
              (one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)) 
              (one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)) 
              (one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)) 
              (one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole))))


        [dlen1 apcoef1] (apfloor (/ (* (double *sr*) strikeposition) freq) wt)

        adelout
        (one-pole-allpass (adelay adelin dlen1) apcoef1)

        combedexcitationsignal
        (shared 
          (mul hammergain
               (sum adelout
                    (mul adelin strikepositioninvfac))))


        ;;compute coefficients for and initialize the coupling filter
        ;;taking l=g(1 - bz^-1)/(1-b), and computing hb = -(1-l)/(2-l)
        attenuationperperiod
        (Math/pow 10.0 (/ singlestringdecayrate freq 20.0))

        g attenuationperperiod ;;dc gain
        b singlestringzero
        a singlestringpole
        ctemp (+ 1.0 (- b) g (- (* a g))
                 (* (double nstrings) (+ 1.0 (- b) (- g) (* a g))))

        cfb0 (/ (* 2.0 (+ -1.0 b g (- (* a g)))) ctemp)
        cfb1 (/ (* 2.0 (+ a (- (* a b)) (- (* b g)) (* a b g))) ctemp)
        cfa1 (/ (+ (- a) (* a b) (- (* b g)) (* a b g)
                   (* (double nstrings) (+ (- a) (* a b) (* b g) (- (* a b g)))))
                ctemp)


        ;;determine string tunings (and longitudinal modes, if present)
        freq1
        (if (<= keynum (double longitudinal-mode-cutoff-keynum))
          (* freq (double longitudinalmode)) freq)
        freq2 (+ freq (* detuning2 (double detuningfactor)))
        freq3 (+ freq (* detuning3 (double detuningfactor)))


        ;;scale stiffness coefficients, if desired
        stiffnesscoefficient
        (if (> stiffnessfactor 1.0)
          (- stiffnesscoefficient
             (* (+ stiffnesscoefficient 1.0)
                (- stiffnessfactor 1.0)))
          (* stiffnesscoefficient stiffnessfactor))
        stiffnesscoefficientl
        (if (<= keynum (double longitudinal-mode-cutoff-keynum))
          longitudinal-mode-stiffness-coefficient
          stiffnesscoefficient)

        [delaylength1 tuningcoefficient1] 
        (tune-piano freq1 stiffnesscoefficientl 
                    number-of-stiffness-allpasses cfb0 cfb1 cfa1)
        [delaylength2 tuningcoefficient2] 
        (tune-piano freq2 stiffnesscoefficient 
                    number-of-stiffness-allpasses cfb0 cfb1 cfa1)

        [delaylength3 tuningcoefficient3] 
        (tune-piano freq3 stiffnesscoefficient 
                    number-of-stiffness-allpasses cfb0 cfb1 cfa1)

        out (create-buffer)

        ^IFn$DD string1-delay 
        (make-string-delay delaylength1 tuningcoefficient1 stiffnesscoefficientl)

        ^IFn$DD string2-delay 
        (make-string-delay delaylength2 tuningcoefficient2 stiffnesscoefficient)

        ^IFn$DD string3-delay 
        (make-string-delay delaylength3 tuningcoefficient3 stiffnesscoefficient)

        ^IFn$DD coupling-filter (ss-one-pole-one-zero cfb0 cfb1 cfa1) 

        loop-gain (hold-until 
                    duration loop-gain-default
                    (expseg loop-gain-default releaseloopgain
                            (in-t60 loop-gain-env-t60) :release))

]

(generator
  [loop-gain loop-gain-default
   s1-junction-input 0.0
   s2-junction-input 0.0
   s3-junction-input 0.0
   coupling-filter-output 0.0]
  [sig combedexcitationsignal
   loop-gain-val loop-gain]
  (let [new-s1 (+ (* unacordagain sig) 
                  (* loop-gain-val
                     (.invokePrim string1-delay (+ coupling-filter-output s1-junction-input))))
        new-s2 (+ sig 
                  (* loop-gain-val 
                     (.invokePrim string2-delay (+ coupling-filter-output s2-junction-input))))

        new-s3 (+ sig 
                  (* loop-gain-val 
                     (.invokePrim string3-delay (+ coupling-filter-output s3-junction-input))))

        coupling-filt-in (+ new-s1 new-s2 new-s3)
        coupling-filt-out (.invokePrim coupling-filter coupling-filt-in)
        ]

    (aset out int-indx coupling-filt-in)
    (gen-recur loop-gain new-s1 new-s2 new-s3 coupling-filt-out))


  (yield out)
  )))      



