(ns pink.instruments.piano
  "Translation of Scott Van Duyne's Piano Model from Common Lisp Music"
  (:require [clojure.math.numeric-tower :refer [expt]]
            [pink.util :refer :all]
            [pink.config :refer :all]))

(def number-of-stiffness-allpasses 8)
(def longitudinal-mode-cutoff-keynum 29)
(def longitudinal-mode-stiffness-coefficient -.5)
(def golden-mean .618)
(def loop-gain-env-t60 .05)
(def loop-gain-default .9999)
(def nstrings 3)
(def TWO-PI (* 2 Math/PI))

;;keynum indexed parameter tables
;;these should all be &key variable defaults for p instrument

(def default-loudpole-table
  '(36 .8 60 .85 84 .7 96 .6 108 .5))

(def default-softpole-table
  '(36 .93 60 .9 84 .9 96 .8 108 .8))

(def default-loudgain-table
  '(21.000 0.700 36.000 0.700 48.000 0.700 60.000 0.650 72.000 0.650 84.000 0.650 87.006 0.681 88.070 0.444 90.653 0.606 95.515 0.731 99.770 0.775 101.897 0.794 104.024 0.800 105.695 0.806))

(def default-softgain-table
  '(21 .25 108 .25))

(def default-strikeposition-table
  '(21.000 0.140 23.884 0.139 36.000 0.128 56.756 0.129 57.765 0.130 59.000 0.130 60.000 0.128 61.000 0.128 62.000 0.129 66.128 0.129 69.000 0.128 72.000 0.128 73.000 0.128 79.000 0.128 80.000 0.128 96.000 0.128 99.000 0.128))

(def default-detuning2-table
  '(22.017 -0.090 23.744 -0.090 36.000 -0.080 48.055 -0.113 60.000 -0.135 67.264 -0.160 72.000 -0.200 84.054 -0.301 96.148 -0.383 108 -0.383))

(def default-detuning3-table '(21.435 0.027 23.317 0.043 36.000 0.030 48.000 0.030 60.000 0.030 72.000 0.020 83.984 0.034 96.000 0.034 99.766 0.034))

(def default-stiffnesscoefficient-table
  '(21.000 -0.920 24.000 -0.900 36.000 -0.700 48.000 -0.250 60.000 -0.100 75.179 -0.040 82.986 -0.040 92.240 -0.040 96.000 -0.040 99.000 .2 108.000 .5))

(def default-singlestringdecayrate-table
  '(21.678 -2.895 24.000 -3.000 36.000 -4.641 41.953 -5.867 48.173 -7.113 53.818 -8.016 59.693 -8.875 66.605 -9.434 73.056 -10.035 78.931 -10.293 84.000 -12.185))

(def default-singlestringzero-table
  '(21.000 -0.300 24.466 -0.117 28.763 -0.047 36.000 -0.030 48.000 -0.020 60.000 -0.010 72.000 -0.010 84.000 -0.010 96.000 -0.010))

(def default-singlestringpole-table
  '(21.000 0 24.466 0 28.763 0 36.000 0 108 0))

(def default-releaseloopgain-table
  '(21.643 0.739 24.000 0.800 36.000 0.880 48.000 0.910 60.000 0.940 72.000 0.965 84.000 0.987 88.99 .987 89.0 1.0 108 1.0))

(def default-drytapfiltcoeft60-table
  '(36 .35 60 .25 108 .15))

(def default-drytapfiltcoeftarget-table
  '(36 -.8 60 -.5 84 -.4 108 -.1))

(def default-drytapfiltcoefcurrent-table
  '(0 0 200 0))

(def default-drytapampt60-table
  '(36 .55 60 .5 108 .45))

(def default-sustainpedallevel-table
  '(21.000 0.250 24.000 0.250 36.000 0.200 48.000 0.125 60.000 0.075 72.000 0.050 84.000 0.030 96.000 0.010 99.000 0.010))

(def default-pedalresonancepole-table
  '(20.841 0.534 21.794 0.518 33.222 0.386 45.127 0.148 55.445 -0.065 69.255 -0.409 82.905 -0.729 95.763 -0.869 106.398 -0.861))

(def default-pedalenvelopet60-table
  '(21.0 7.5 108.0 7.5))

(def default-soundboardcutofft60-table
  '(21.0 .25 108.0 .25))

(def default-drypedalresonancefactor-table
  '(21.0 .5 108.0 .5))

(def default-unacordagain-table
  '(21 1.0  24 .4 29 .1 29.1 .95 108 .95))


;; converts t60 values to suitable :rate values for expseg
(defn in-t60 [t60] 
  (- 1.0 (expt 0.001 (/ 1.0 t60 *sr*))))

;;; expseg (like musickit asymp)
;(def-clm-struct expsegstr currentvalue targetvalue rate)
;(defn make-expseg (&key (currentvalue 0.0) (targetvalue 0.0) (rate .5))
;  (make-expsegstr
;   :currentvalue currentvalue :targetvalue targetvalue :rate rate))
;(defmacro expseg (f)
;  `(let ((cv (expsegstr-currentvalue ,f)))
;     (setf (expsegstr-currentvalue ,f)
;     (+ cv (* (- (expsegstr-targetvalue ,f) cv) (expsegstr-rate ,f))))))

;(deftype ExpSegStr [^:cur-val target-val rate])
;(defn make-expseg
;  [& {:keys [cur-val target-val rate]
;      :or {cur-val 0.0 target-val 0.0 rate 0.5}}]
;  (ExpSegStr. cur-val target-val rate))
;(defn expseg
;  [^ExpSegStr f]
;  )

;(def-clm-struct simpfilt a0 a1 a2 b0 b1 b2 x1 x2 y1 y2)

;;;; signal controlled one-pole lowpass filter
;(defn make-one-pole-swept (&key (y1 0.0)) (make-simpfilt :y1 y1))
;(defmacro one-pole-swept (f input coef)
;  `(let* ((coef ,coef)
;    (output (- (* (1+ coef) ,input) (* coef (simpfilt-y1 ,f)))))
;     (setf (simpfilt-y1 ,f) output)))

;(defn one-pole-swept
;  [f input coef]
;  (let [out (create-buffer)
        
;        ])
;  )

;;;; one-pole allpass filter
;(defn make-one-pole-allpass (coef &key (x1 0.0) (y1 0.0))
;  (make-simpfilt :a0 coef :x1 x1 :y1 y1))
;(defmacro one-pole-allpass (f input)
;  `(let* ((input ,input)
;    (output (+ (* (simpfilt-a0 ,f) (- input (simpfilt-y1 ,f)))
;         (simpfilt-x1 ,f))))
;     (setf (simpfilt-x1 ,f) input
;     (simpfilt-y1 ,f) output)))


;;;; one-pole-one-zero filter:  y(n) = a0 x(n) + a1 x(n-1) - b0 y(n-1)
;(defn make-one-pole-one-zero (a0 a1 b1 &key (x1 0.0) (y1 0.0))
;  (make-simpfilt :a0 a0 :a1 a1 :b1 b1 :x1 x1 :y1 y1))
;(defmacro one-pole-one-zero (f input)
;  `(let* ((input ,input)
;    (output (- (+ (* (simpfilt-a0 ,f) input)
;      (* (simpfilt-a1 ,f) (simpfilt-x1 ,f)))
;         (* (simpfilt-b1 ,f) (simpfilt-y1 ,f)))))
;     (setf (simpfilt-x1 ,f) input
;     (simpfilt-y1 ,f) output)))

;;;;very special noise generator
;(def-clm-struct noisestr noise-seed)
;(defn make-noise (&key (noise-seed 16383))
;  (make-noisestr :noise-seed noise-seed))
;(defmacro noise (r amp)
;  `(let
;      ((seed (+ (* (noisestr-noise-seed ,r) 1103515245)
;             12345)))
;    (setf (noisestr-noise-seed ,r) seed)
;    (* ,amp (- (* (mod (/ seed
;              65536) 65536) 0.0000305185) 1.0))))

;(defn noise
;  "\"very special noise generator\" from piano.clm"
;  [amp]
;  (let [out ^doubles (create-buffer)
;        seed-state (atom 16383)]
;    (fn []
;      (loop [i 0 seed @seed-state]
;        (if (< i *buffer-size*)
;          (let [new-seed (+ (* seed 1103515245) 12345)
;                new-val (* amp 
;                           (- (* (mod (/ new-seed 65536) 65536) 0.0000305185)
;                              1.0))]
;            (aset out i (double new-val))
;            (recur (unchecked-inc i) new-seed))
;          (do
;            (reset! seed-state seed)
;            out))))))
;(def t (noise 0.5))
;(require '[clojure.pprint :refer [pprint]])
;(try (t) (catch Exception e (.printStackTrace e)))

;;;;delay line unit generator with length 0 capabilities...
;(defn make-delay0 (len)
;  (cond ((> len 0) (make-delay len))
;      ((= len 0) nil)
;      (t (clm-print "can't handle that much stiffness on current pitch") nil)))
;(defmacro delay0 (f input)
;  `(let ((input ,input))
;     (if ,f (delay ,f input) input)))

(defn apphase [a1 wt]
  (Math/atan2 (* (- (* a1 a1) 1.0) (Math/sin wt))
        (+ (* 2.0 a1) (*(+ (* a1 a1) 1.0) (Math/cos wt)))))

(defn opozphase [b0 b1 a1 wt] 
  (let [s (Math/sin wt)
        c (Math/cos wt)] 
    (Math/atan2 (- (* a1 s (+ b0 (* b1 c))) (* b1 s (+ 1 (* a1 c))))
          (+ (* (+ b0 (* b1 c)) (+ 1 (* a1 c))) (* b1 s a1 s)))))

(defn get-allpass-coef [samp-frac wt]
  (let [ta (Math/tan (- (* samp-frac wt)))
        c (Math/cos wt)
        s (Math/sin wt)]
    (/ (+ (- ta) (* (Math/signum ta)
                    (Math/sqrt (* (+ 1 (* ta ta)) (* s s)))))
       (- (* c ta) s))))

(defn apfloor [len wt]
  (multiple-value-bind
    (len-int len-frac) (floor len)
    (if (< len-frac golden-mean)
      (let () (decf len-int)(incf len-frac)))
    (and (< len-frac golden-mean)
         (> len-int 0)
         (let () (decf len-int)(incf len-frac)))
    (values len-int (get-allpass-coef len-frac wt))))

(defn tune-piano
  [frequency stiffnesscoefficient numallpasses b0 b1 a1] 
  (let [wt (/ (* frequency two-pi) *srate*)
        len (/ (+ TWO-PI 
                (* numallpasses
                   (apphase stiffnesscoefficient wt))
                (opozphase (+ 1 (* 3 b0)) (+ a1 (* 3 b1)) a1 wt))
             wt)]
    (apfloor len wt)))

(defn envelope-interp [keynum table]
 :envelope-interp 
  )

(defn arg-lookup 
  [args kwd keynum table]
  (if-let [v (kwd args)] 
    v
    (envelope-interp keynum table)))

(defn piano 
  [& {:keys 
      [ duration keyNum strike-velocity 
       pedal-down release-time-margin amp 
       detuningfactor detuningfactor-table  stiffnessfactor 
       stiffnessfactor-table pedalpresencefactor longitudinalmode 
       strikepositioninvfac singlestringdecayratefactor]
      :or {
           duration 1.0
           keyNum 60.0                     ; middleC=60: can use fractional part to detune 
           strike-velocity 0.5            ; corresponding normalized velocities (range: 0.0--1.0)
           pedal-down false                 ; set to #t for sustain pedal down...pedal-down-times not yet implemented
           release-time-margin 0.75      ; extra compute time allowed beyond duration
           amp .5                        ; amp scale of noise inputs...

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

  (let [loudpole (arg-lookup args :loudpole keynum default-loudpole-table)
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
        unacordagain (arg-lookup args :unacordagain keynum default-unacordagain-table)]

    ))



(definstrument p

  (labels
   ((apphase (a1 wt)
	     (atan (* (- (* a1 a1) 1.0) (sin wt))
		   (+ (* 2.0 a1) (*(+ (* a1 a1) 1.0) (cos wt)))))
    (opozphase (b0 b1 a1 wt)
	       (let ((s (sin wt))
		     (c (cos wt)))
		 (atan (- (* a1 s (+ b0 (* b1 c))) (* b1 s (+ 1 (* a1 c))))
		       (+ (* (+ b0 (* b1 c)) (+ 1 (* a1 c))) (* b1 s a1 s)))))
    (get-allpass-coef (samp-frac wt)
		      (let ((ta (tan (- (* samp-frac wt))))
			    (c (cos wt))
			    (s (sin wt)))
			(/ (+ (- ta) (* (signum ta)
			                (sqrt (* (1+ (* ta ta)) (* s s)))))
			   (- (* c ta) s))))
    (apfloor (len wt)
	     (multiple-value-bind
	      (len-int len-frac) (floor len)
	      (if (< len-frac golden-mean)
	          (let () (decf len-int)(incf len-frac)))
	      (and (< len-frac golden-mean)
		   (> len-int 0)
		   (let () (decf len-int)(incf len-frac)))
	      (values len-int (get-allpass-coef len-frac wt))))
    (tune-piano (frequency stiffnesscoefficient numallpasses b0 b1 a1)
		(let*
		    ((wt (/ (* frequency two-pi) *srate*))
		     (len (/ (+ two-pi
				(* numallpasses
				   (apphase stiffnesscoefficient wt))
				(opozphase
				 (+ 1 (* 3 b0)) (+ a1 (* 3 b1)) a1 wt))
			     wt)))
		  (apfloor len wt))))
 
   (let*
       ((beg (floor (* start *srate*)))
	(end (+ beg (floor (* (+ duration release-time-margin)
			      *srate*))))
	(dur (floor (* duration *srate*)))
	(freq (* 440.0 (expt 2.0 (/ (- keynum 69.0) 12.0))))
	(wt (/ (* two-pi freq) *srate*))
	
	
	;;initialize locsig
;;;	(loc (make-locsig :distance distance :degree degree :reverb reverb-amount))

	;;initialize soundboard impulse response elements
	(drytap-one-pole-one-zero
	 (make-one-pole-one-zero 1.0 0.0 0.0))
	(drytap-coef-expseg
	 (make-expseg
	  :currentvalue drytapfiltcoefcurrent
	  :targetvalue drytapfiltcoeftarget
	  :rate (in-t60 drytapfiltcoeft60)))
	(drytap-one-pole-swept
	 (make-one-pole-swept))
	(drytap-amp-expseg
	 (make-expseg
	  :currentvalue 1.0
	  :targetvalue 0.0
	  :rate (in-t60 drytapampt60)))
	
	;;initialize open-string resonance elements		
	(wettap-one-pole-one-zero
	 (make-one-pole-one-zero
	  (- 1.0 (* (signum pedalresonancepole) pedalresonancepole))
	  0.0
	  (- pedalresonancepole)))
	(wettap-coef-expseg
	 (make-expseg
	  :currentvalue 0.0
	  :targetvalue -.5
	  :rate (in-t60 pedalenvelopet60)))
	(wettap-one-pole-swept
	 (make-one-pole-swept))
	(wettap-amp-expseg
	 (make-expseg
	  :currentvalue (* sustainpedallevel
			   pedalpresencefactor
			   (if pedal-down 1.0 drypedalresonancefactor))
	  :targetvalue 0.0
	  :rate (in-t60 pedalenvelopet60)))
	(sb-cutoff-rate
	 (in-t60 soundboardcutofft60))
	
	;;initialize velocity-dependent piano hammer filter elements
	(hammerpole
	 (+ softpole (* (- loudpole softpole) strike-velocity)))
	(hammergain
	 (+ softgain (* (- loudgain softgain) strike-velocity)))
	(hammer-one-pole1
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	(hammer-one-pole2
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	(hammer-one-pole3
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	(hammer-one-pole4
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	
	;;strike position comb filter delay length
	(agraffe-len (/ (* *srate* strikeposition) freq)))
     (multiple-value-bind
      (dlen1 apcoef1) (apfloor agraffe-len wt)
	(let*
	    ((agraffe-delay1 (make-delay0 dlen1))
	     (agraffe-tuning-ap1 (make-one-pole-allpass apcoef1))
	   
	     ;;compute coefficients for and initialize the coupling filter
	     ;;taking l=g(1 - bz^-1)/(1-b), and computing hb = -(1-l)/(2-l)
	     (attenuationperperiod
	      (expt 10.0 (/ singlestringdecayrate freq 20.0)))
	     (g attenuationperperiod)  ;;dc gain
	     (b singlestringzero)
			 (a singlestringpole)
			 (ctemp (+ 1 (- b) g (- (* a g))
									(* nstrings (+ 1 (- b) (- g) (* a g)))))

			 (cfb0 (/ (* 2 (+ -1 b g (- (* a g)))) ctemp))
			 (cfb1 (/ (* 2 (+ a (- (* a b)) (- (* b g)) (* a b g))) ctemp))
			 (cfa1 (/ (+ (- a) (* a b) (- (* b g)) (* a b g)
										(* nstrings (+ (- a) (* a b) (* b g) (- (* a b g)))))
								 ctemp))
			 (couplingfilter (make-one-pole-one-zero cfb0 cfb1 cfa1))
	   
	     ;;determine string tunings (and longitudinal modes, if present)
	     (freq1
	      (if (<= keynum longitudinal-mode-cutoff-keynum)
		  (* freq longitudinalmode) freq))
	     (freq2 (+ freq (* detuning2 detuningfactor)))
	     (freq3 (+ freq (* detuning3 detuningfactor)))
	   
	     ;;scale stiffness coefficients, if desired
	     (stiffnesscoefficient
	      (if (> stiffnessfactor 1.0)
		  (- stiffnesscoefficient
		     (* (1+ stiffnesscoefficient)
			(1- stiffnessfactor)))
		(* stiffnesscoefficient stiffnessfactor)))
	     (stiffnesscoefficientl
	      (if (<= keynum longitudinal-mode-cutoff-keynum)
		  longitudinal-mode-stiffness-coefficient
		stiffnesscoefficient)))
	
	  ;;initialize the coupled-string elements
	  (multiple-value-bind
	   (delaylength1 tuningcoefficient1)
	   (tune-piano
	    freq1 stiffnesscoefficientl
	    number-of-stiffness-allpasses
	    cfb0 cfb1 cfa1)
	   (multiple-value-bind
	    (delaylength2 tuningcoefficient2)
	    (tune-piano
	     freq2 stiffnesscoefficient
	     number-of-stiffness-allpasses
	     cfb0 cfb1 cfa1)
	    (multiple-value-bind
	     (delaylength3 tuningcoefficient3)
	     (tune-piano
	      freq3 stiffnesscoefficient
	      number-of-stiffness-allpasses
	      cfb0 cfb1 cfa1)
	     (let*
		 ((string1-delay
		   (make-delay0 (1- delaylength1)))
		  (string1-tuning-ap
		   (make-one-pole-allpass tuningcoefficient1))
		  (string1-stiffness-ap1
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap2
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap3
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap4
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap5
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap6
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap7
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap8
		   (make-one-pole-allpass stiffnesscoefficientl))
		
		  (string2-delay
		   (make-delay0 (1- delaylength2)))
		  (string2-tuning-ap
		   (make-one-pole-allpass tuningcoefficient2))
		  (string2-stiffness-ap1
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap2
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap3
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap4
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap5
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap6
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap7
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap8
		   (make-one-pole-allpass stiffnesscoefficient))
		
		  (string3-delay
		   (make-delay0 (1- delaylength3)))
		  (string3-tuning-ap
		   (make-one-pole-allpass tuningcoefficient3))
		  (string3-stiffness-ap1
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap2
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap3
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap4
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap5
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap6
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap7
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap8
		   (make-one-pole-allpass stiffnesscoefficient))
		
		  ;;initialize loop-gain envelope
		  (loop-gain-expseg
		   (make-expseg
		    :currentvalue loop-gain-default
		    :targetvalue releaseloopgain
		    :rate (in-t60 loop-gain-env-t60)))
		
		  (drytap 0.0)
		  (openstrings 0.0)
		  (combedexcitationsignal 0.0)
		  (adelout 0.0)
		  (adelin 0.0)
		  (totaltap 0.0)
		  (loop-gain loop-gain-default)
		  (is-release-time nil)
		  (string1-junction-input 0.0)
		  (string2-junction-input 0.0)
		  (string3-junction-input 0.0)
		  (couplingfilter-input 0.0)
		  (couplingfilter-output 0.0)
		  (sampcount 0)
			(noi (make-noise)))
	       (run
		(loop
		 for i from beg to end
		 do
		 (cond
		  (is-release-time
		   (setf loop-gain (expseg loop-gain-expseg)))
		  ((= sampcount dur)
		   (setf
		    is-release-time t
		    (expsegstr-rate drytap-amp-expseg) sb-cutoff-rate
		    (expsegstr-rate wettap-amp-expseg) sb-cutoff-rate)))
		
		 (setf
		  drytap
		  (* (expseg
		      drytap-amp-expseg)
		     (one-pole-swept
		      drytap-one-pole-swept
		      (one-pole-one-zero
		       drytap-one-pole-one-zero
		       (noise noi amp))
		      (expseg
		       drytap-coef-expseg)))
		
		  openstrings
		  (* (expseg
		      wettap-amp-expseg)
		     (one-pole-swept
		      wettap-one-pole-swept
		      (one-pole-one-zero
		       wettap-one-pole-one-zero
		       (noise noi amp))
		      (expseg
		       wettap-coef-expseg)))
		
			totaltap
			 (+ drytap openstrings)
			
		adelin
			(one-pole
				hammer-one-pole1
				(one-pole
					hammer-one-pole2
		  	  (one-pole
		 		 	  hammer-one-pole3
		  		  (one-pole
		  			   hammer-one-pole4
							 totaltap ))))
							
					
		combedexcitationsignal
 		(* hammergain
			(+ adelout
			(* adelin strikepositioninvfac)))
		
		adelout
			(one-pole-allpass
			 agraffe-tuning-ap1
			 (delay0
			  agraffe-delay1
				adelin))
					
		  string1-junction-input
		  (+ (* unacordagain combedexcitationsignal)
		     (* loop-gain
			(delay0
			 string1-delay
			 (one-pole-allpass
			  string1-tuning-ap
			  (one-pole-allpass
			   string1-stiffness-ap1
			   (one-pole-allpass
			    string1-stiffness-ap2
			    (one-pole-allpass
			     string1-stiffness-ap3
			    (one-pole-allpass
			     string1-stiffness-ap4
			    (one-pole-allpass
			     string1-stiffness-ap5
			    (one-pole-allpass
			     string1-stiffness-ap6
			    (one-pole-allpass
			     string1-stiffness-ap7
			    (one-pole-allpass
			     string1-stiffness-ap8
			     (+ couplingfilter-output
				string1-junction-input)))))))))))))
		
		  string2-junction-input
		  (+ combedexcitationsignal
		     (* loop-gain
			(delay0
			 string2-delay
			 (one-pole-allpass
			  string2-tuning-ap
			  (one-pole-allpass
			   string2-stiffness-ap1
			   (one-pole-allpass
			    string2-stiffness-ap2
			    (one-pole-allpass
			     string2-stiffness-ap3
			    (one-pole-allpass
			     string2-stiffness-ap4
			    (one-pole-allpass
			     string2-stiffness-ap5
			    (one-pole-allpass
			     string2-stiffness-ap6
			    (one-pole-allpass
			     string2-stiffness-ap7
			    (one-pole-allpass
			     string2-stiffness-ap8
			     (+ couplingfilter-output
				string2-junction-input)))))))))))))
		
		  string3-junction-input
		  (+ combedexcitationsignal
		     (* loop-gain
			(delay0
			 string3-delay
			 (one-pole-allpass
			  string3-tuning-ap
			  (one-pole-allpass
			   string3-stiffness-ap1
			   (one-pole-allpass
			    string3-stiffness-ap2
			    (one-pole-allpass
			     string3-stiffness-ap3
			    (one-pole-allpass
			     string3-stiffness-ap4
			    (one-pole-allpass
			     string3-stiffness-ap5
			    (one-pole-allpass
			     string3-stiffness-ap6
			    (one-pole-allpass
			     string3-stiffness-ap7
			    (one-pole-allpass
			     string3-stiffness-ap8
			     (+ couplingfilter-output
				string3-junction-input)))))))))))))
		
		  couplingfilter-input
		  (+ string1-junction-input
		     string2-junction-input
		     string3-junction-input)
		
		  couplingfilter-output
		  (one-pole-one-zero
		   couplingfilter
		   couplingfilter-input))
;;;		 (locsig loc i couplingfilter-input)
		 (outa i couplingfilter-input)
		 (incf sampcount)))
	       )))))))))

;; common lisp version
(definstrument p
   (start
   &key (duration 1.0)
   (keynum 60.0) ;;middlec=60: can use fractional part to detune
   (strike-velocity 0.5) ;;corresponding normalized velocities (range: 0.0--1.0)
   (pedal-down nil) ;;set to t for sustain pedal down...pedal-down-times not yet impl.
   (release-time-margin 0.75) ;;extra compute time allowed beyond duration
   (amp .5) ;;amp scale of noise inputs...
 
   ;;slider controls
   (detuningfactor 1.0)
	 (detuningfactor-table '())
   (stiffnessfactor 1.0)
	 (stiffnessfactor-table '())
   (pedalpresencefactor .3)
   (longitudinalmode 10.5)
   (strikepositioninvfac -0.9)
   (singlestringdecayratefactor 1.0)
;;;	 (degree 0.0)
;;;	 (distance 1.0)
;;;	 (reverb-amount 0.01)
	
   ;; parameter tables indexed by keynum
   ;; nb: you can override the loudpole-table by directly setting :loudpole to a value
   loudpole
   (loudpole-table default-loudpole-table)
   softpole
   (softpole-table default-softpole-table)
   loudgain
   (loudgain-table default-loudgain-table)
   softgain
   (softgain-table default-softgain-table)
   strikeposition (strikeposition-table default-strikeposition-table)
   detuning2
   (detuning2-table default-detuning2-table)
   detuning3
   (detuning3-table default-detuning3-table)
   stiffnesscoefficient
   (stiffnesscoefficient-table default-stiffnesscoefficient-table)
   singlestringdecayrate
   (singlestringdecayrate-table default-singlestringdecayrate-table)
   singlestringzero
   (singlestringzero-table default-singlestringzero-table)
   singlestringpole
   (singlestringpole-table default-singlestringpole-table)
   releaseloopgain
   (releaseloopgain-table default-releaseloopgain-table)
   drytapfiltcoeft60
   (drytapfiltcoeft60-table default-drytapfiltcoeft60-table)
   drytapfiltcoeftarget
   (drytapfiltcoeftarget-table default-drytapfiltcoeftarget-table)
   drytapfiltcoefcurrent
   (drytapfiltcoefcurrent-table default-drytapfiltcoefcurrent-table)
   drytapampt60
   (drytapampt60-table default-drytapampt60-table)
   sustainpedallevel
   (sustainpedallevel-table default-sustainpedallevel-table)
   pedalresonancepole
   (pedalresonancepole-table default-pedalresonancepole-table)
   pedalenvelopet60
   (pedalenvelopet60-table default-pedalenvelopet60-table)
   soundboardcutofft60
   (soundboardcutofft60-table default-soundboardcutofft60-table)
   drypedalresonancefactor
   (drypedalresonancefactor-table default-drypedalresonancefactor-table)
   unacordagain
   (unacordagain-table default-unacordagain-table)
   )

  (labels
   ((apphase (a1 wt)
	     (atan (* (- (* a1 a1) 1.0) (sin wt))
		   (+ (* 2.0 a1) (*(+ (* a1 a1) 1.0) (cos wt)))))
    (opozphase (b0 b1 a1 wt)
	       (let ((s (sin wt))
		     (c (cos wt)))
		 (atan (- (* a1 s (+ b0 (* b1 c))) (* b1 s (+ 1 (* a1 c))))
		       (+ (* (+ b0 (* b1 c)) (+ 1 (* a1 c))) (* b1 s a1 s)))))
    (get-allpass-coef (samp-frac wt)
		      (let ((ta (tan (- (* samp-frac wt))))
			    (c (cos wt))
			    (s (sin wt)))
			(/ (+ (- ta) (* (signum ta)
			                (sqrt (* (1+ (* ta ta)) (* s s)))))
			   (- (* c ta) s))))
    (apfloor (len wt)
	     (multiple-value-bind
	      (len-int len-frac) (floor len)
	      (if (< len-frac golden-mean)
	          (let () (decf len-int)(incf len-frac)))
	      (and (< len-frac golden-mean)
		   (> len-int 0)
		   (let () (decf len-int)(incf len-frac)))
	      (values len-int (get-allpass-coef len-frac wt))))
    (tune-piano (frequency stiffnesscoefficient numallpasses b0 b1 a1)
		(let*
		    ((wt (/ (* frequency two-pi) *srate*))
		     (len (/ (+ two-pi
				(* numallpasses
				   (apphase stiffnesscoefficient wt))
				(opozphase
				 (+ 1 (* 3 b0)) (+ a1 (* 3 b1)) a1 wt))
			     wt)))
		  (apfloor len wt))))
 
   (let*
       ((beg (floor (* start *srate*)))
	(end (+ beg (floor (* (+ duration release-time-margin)
			      *srate*))))
	(dur (floor (* duration *srate*)))
	(freq (* 440.0 (expt 2.0 (/ (- keynum 69.0) 12.0))))
	(wt (/ (* two-pi freq) *srate*))
	
	;;look-up parameters in tables (or else use the override value)
	(loudpole
	 (or loudpole
	     (envelope-interp keynum loudpole-table)))
	(softpole
	 (or softpole
	     (envelope-interp keynum softpole-table)))
	(loudgain
	 (or loudgain
	     (envelope-interp keynum loudgain-table)))
	(softgain
	 (or softgain
	     (envelope-interp keynum softgain-table)))
	(strikeposition
	 (or strikeposition
	     (envelope-interp keynum strikeposition-table)))
	(detuning2
	 (or detuning2
	     (envelope-interp keynum detuning2-table)))
	(detuning3
	 (or detuning3
	     (envelope-interp keynum detuning3-table)))
	(stiffnesscoefficient
	 (or stiffnesscoefficient
	     (envelope-interp keynum stiffnesscoefficient-table)))
	(singlestringdecayrate
	 (or singlestringdecayrate
	     (envelope-interp keynum singlestringdecayrate-table)))
	(singlestringdecayrate (* singlestringdecayratefactor
				  singlestringdecayrate))
	(singlestringzero
	 (or singlestringzero
	     (envelope-interp keynum singlestringzero-table)))
	(singlestringpole
	 (or singlestringpole
	     (envelope-interp keynum singlestringpole-table)))
	(releaseloopgain
	 (or releaseloopgain
	     (envelope-interp keynum releaseloopgain-table)))
	(drytapfiltcoeft60
	 (or drytapfiltcoeft60
	     (envelope-interp keynum drytapfiltcoeft60-table)))
	(drytapfiltcoeftarget
	 (or drytapfiltcoeftarget
	     (envelope-interp keynum drytapfiltcoeftarget-table)))
	(drytapfiltcoefcurrent
	 (or drytapfiltcoefcurrent
	     (envelope-interp keynum drytapfiltcoefcurrent-table)))
	(drytapampt60
	 (or drytapampt60
	     (envelope-interp keynum drytapampt60-table)))
	(sustainpedallevel
	 (or sustainpedallevel
	     (envelope-interp keynum sustainpedallevel-table)))
	(pedalresonancepole
	 (or pedalresonancepole
	     (envelope-interp keynum pedalresonancepole-table)))
	(pedalenvelopet60
	 (or pedalenvelopet60
	     (envelope-interp keynum pedalenvelopet60-table)))
	(soundboardcutofft60
	 (or soundboardcutofft60
	     (envelope-interp keynum soundboardcutofft60-table)))
	(drypedalresonancefactor
	 (or drypedalresonancefactor
	     (envelope-interp keynum drypedalresonancefactor-table)))
	(unacordagain
	 (or unacordagain
	     (envelope-interp keynum unacordagain-table)))
	(detuningfactor
	(if detuningfactor-table
		(envelope-interp keynum detuningfactor-table)
		detuningfactor))
	(stiffnessfactor
	(if stiffnessfactor-table
		(envelope-interp keynum stiffnessfactor-table)
		stiffnessfactor))
	
	;;initialize locsig
;;;	(loc (make-locsig :distance distance :degree degree :reverb reverb-amount))

	;;initialize soundboard impulse response elements
	(drytap-one-pole-one-zero
	 (make-one-pole-one-zero 1.0 0.0 0.0))
	(drytap-coef-expseg
	 (make-expseg
	  :currentvalue drytapfiltcoefcurrent
	  :targetvalue drytapfiltcoeftarget
	  :rate (in-t60 drytapfiltcoeft60)))
	(drytap-one-pole-swept
	 (make-one-pole-swept))
	(drytap-amp-expseg
	 (make-expseg
	  :currentvalue 1.0
	  :targetvalue 0.0
	  :rate (in-t60 drytapampt60)))
	
	;;initialize open-string resonance elements		
	(wettap-one-pole-one-zero
	 (make-one-pole-one-zero
	  (- 1.0 (* (signum pedalresonancepole) pedalresonancepole))
	  0.0
	  (- pedalresonancepole)))
	(wettap-coef-expseg
	 (make-expseg
	  :currentvalue 0.0
	  :targetvalue -.5
	  :rate (in-t60 pedalenvelopet60)))
	(wettap-one-pole-swept
	 (make-one-pole-swept))
	(wettap-amp-expseg
	 (make-expseg
	  :currentvalue (* sustainpedallevel
			   pedalpresencefactor
			   (if pedal-down 1.0 drypedalresonancefactor))
	  :targetvalue 0.0
	  :rate (in-t60 pedalenvelopet60)))
	(sb-cutoff-rate
	 (in-t60 soundboardcutofft60))
	
	;;initialize velocity-dependent piano hammer filter elements
	(hammerpole
	 (+ softpole (* (- loudpole softpole) strike-velocity)))
	(hammergain
	 (+ softgain (* (- loudgain softgain) strike-velocity)))
	(hammer-one-pole1
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	(hammer-one-pole2
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	(hammer-one-pole3
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	(hammer-one-pole4
	 (make-one-pole (* 1.0 (- 1.0 hammerpole)) (- hammerpole)))
	
	;;strike position comb filter delay length
	(agraffe-len (/ (* *srate* strikeposition) freq)))
     (multiple-value-bind
      (dlen1 apcoef1) (apfloor agraffe-len wt)
	(let*
	    ((agraffe-delay1 (make-delay0 dlen1))
	     (agraffe-tuning-ap1 (make-one-pole-allpass apcoef1))
	   
	     ;;compute coefficients for and initialize the coupling filter
	     ;;taking l=g(1 - bz^-1)/(1-b), and computing hb = -(1-l)/(2-l)
	     (attenuationperperiod
	      (expt 10.0 (/ singlestringdecayrate freq 20.0)))
	     (g attenuationperperiod)  ;;dc gain
	     (b singlestringzero)
			 (a singlestringpole)
			 (ctemp (+ 1 (- b) g (- (* a g))
									(* nstrings (+ 1 (- b) (- g) (* a g)))))

			 (cfb0 (/ (* 2 (+ -1 b g (- (* a g)))) ctemp))
			 (cfb1 (/ (* 2 (+ a (- (* a b)) (- (* b g)) (* a b g))) ctemp))
			 (cfa1 (/ (+ (- a) (* a b) (- (* b g)) (* a b g)
										(* nstrings (+ (- a) (* a b) (* b g) (- (* a b g)))))
								 ctemp))
			 (couplingfilter (make-one-pole-one-zero cfb0 cfb1 cfa1))
	   
	     ;;determine string tunings (and longitudinal modes, if present)
	     (freq1
	      (if (<= keynum longitudinal-mode-cutoff-keynum)
		  (* freq longitudinalmode) freq))
	     (freq2 (+ freq (* detuning2 detuningfactor)))
	     (freq3 (+ freq (* detuning3 detuningfactor)))
	   
	     ;;scale stiffness coefficients, if desired
	     (stiffnesscoefficient
	      (if (> stiffnessfactor 1.0)
		  (- stiffnesscoefficient
		     (* (1+ stiffnesscoefficient)
			(1- stiffnessfactor)))
		(* stiffnesscoefficient stiffnessfactor)))
	     (stiffnesscoefficientl
	      (if (<= keynum longitudinal-mode-cutoff-keynum)
		  longitudinal-mode-stiffness-coefficient
		stiffnesscoefficient)))
	
	  ;;initialize the coupled-string elements
	  (multiple-value-bind
	   (delaylength1 tuningcoefficient1)
	   (tune-piano
	    freq1 stiffnesscoefficientl
	    number-of-stiffness-allpasses
	    cfb0 cfb1 cfa1)
	   (multiple-value-bind
	    (delaylength2 tuningcoefficient2)
	    (tune-piano
	     freq2 stiffnesscoefficient
	     number-of-stiffness-allpasses
	     cfb0 cfb1 cfa1)
	    (multiple-value-bind
	     (delaylength3 tuningcoefficient3)
	     (tune-piano
	      freq3 stiffnesscoefficient
	      number-of-stiffness-allpasses
	      cfb0 cfb1 cfa1)
	     (let*
		 ((string1-delay
		   (make-delay0 (1- delaylength1)))
		  (string1-tuning-ap
		   (make-one-pole-allpass tuningcoefficient1))
		  (string1-stiffness-ap1
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap2
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap3
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap4
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap5
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap6
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap7
		   (make-one-pole-allpass stiffnesscoefficientl))
		  (string1-stiffness-ap8
		   (make-one-pole-allpass stiffnesscoefficientl))
		
		  (string2-delay
		   (make-delay0 (1- delaylength2)))
		  (string2-tuning-ap
		   (make-one-pole-allpass tuningcoefficient2))
		  (string2-stiffness-ap1
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap2
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap3
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap4
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap5
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap6
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap7
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string2-stiffness-ap8
		   (make-one-pole-allpass stiffnesscoefficient))
		
		  (string3-delay
		   (make-delay0 (1- delaylength3)))
		  (string3-tuning-ap
		   (make-one-pole-allpass tuningcoefficient3))
		  (string3-stiffness-ap1
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap2
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap3
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap4
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap5
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap6
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap7
		   (make-one-pole-allpass stiffnesscoefficient))
		  (string3-stiffness-ap8
		   (make-one-pole-allpass stiffnesscoefficient))
		
		  ;;initialize loop-gain envelope
		  (loop-gain-expseg
		   (make-expseg
		    :currentvalue loop-gain-default
		    :targetvalue releaseloopgain
		    :rate (in-t60 loop-gain-env-t60)))
		
		  (drytap 0.0)
		  (openstrings 0.0)
		  (combedexcitationsignal 0.0)
		  (adelout 0.0)
		  (adelin 0.0)
		  (totaltap 0.0)
		  (loop-gain loop-gain-default)
		  (is-release-time nil)
		  (string1-junction-input 0.0)
		  (string2-junction-input 0.0)
		  (string3-junction-input 0.0)
		  (couplingfilter-input 0.0)
		  (couplingfilter-output 0.0)
		  (sampcount 0)
			(noi (make-noise)))
	       (run
		(loop
		 for i from beg to end
		 do
		 (cond
		  (is-release-time
		   (setf loop-gain (expseg loop-gain-expseg)))
		  ((= sampcount dur)
		   (setf
		    is-release-time t
		    (expsegstr-rate drytap-amp-expseg) sb-cutoff-rate
		    (expsegstr-rate wettap-amp-expseg) sb-cutoff-rate)))
		
		 (setf
		  drytap
		  (* (expseg
		      drytap-amp-expseg)
		     (one-pole-swept
		      drytap-one-pole-swept
		      (one-pole-one-zero
		       drytap-one-pole-one-zero
		       (noise noi amp))
		      (expseg
		       drytap-coef-expseg)))
		
		  openstrings
		  (* (expseg
		      wettap-amp-expseg)
		     (one-pole-swept
		      wettap-one-pole-swept
		      (one-pole-one-zero
		       wettap-one-pole-one-zero
		       (noise noi amp))
		      (expseg
		       wettap-coef-expseg)))
		
			totaltap
			 (+ drytap openstrings)
			
		adelin
			(one-pole
				hammer-one-pole1
				(one-pole
					hammer-one-pole2
		  	  (one-pole
		 		 	  hammer-one-pole3
		  		  (one-pole
		  			   hammer-one-pole4
							 totaltap ))))
							
					
		combedexcitationsignal
 		(* hammergain
			(+ adelout
			(* adelin strikepositioninvfac)))
		
		adelout
			(one-pole-allpass
			 agraffe-tuning-ap1
			 (delay0
			  agraffe-delay1
				adelin))
					
		  string1-junction-input
		  (+ (* unacordagain combedexcitationsignal)
		     (* loop-gain
			(delay0
			 string1-delay
			 (one-pole-allpass
			  string1-tuning-ap
			  (one-pole-allpass
			   string1-stiffness-ap1
			   (one-pole-allpass
			    string1-stiffness-ap2
			    (one-pole-allpass
			     string1-stiffness-ap3
			    (one-pole-allpass
			     string1-stiffness-ap4
			    (one-pole-allpass
			     string1-stiffness-ap5
			    (one-pole-allpass
			     string1-stiffness-ap6
			    (one-pole-allpass
			     string1-stiffness-ap7
			    (one-pole-allpass
			     string1-stiffness-ap8
			     (+ couplingfilter-output
				string1-junction-input)))))))))))))
		
		  string2-junction-input
		  (+ combedexcitationsignal
		     (* loop-gain
			(delay0
			 string2-delay
			 (one-pole-allpass
			  string2-tuning-ap
			  (one-pole-allpass
			   string2-stiffness-ap1
			   (one-pole-allpass
			    string2-stiffness-ap2
			    (one-pole-allpass
			     string2-stiffness-ap3
			    (one-pole-allpass
			     string2-stiffness-ap4
			    (one-pole-allpass
			     string2-stiffness-ap5
			    (one-pole-allpass
			     string2-stiffness-ap6
			    (one-pole-allpass
			     string2-stiffness-ap7
			    (one-pole-allpass
			     string2-stiffness-ap8
			     (+ couplingfilter-output
				string2-junction-input)))))))))))))
		
		  string3-junction-input
		  (+ combedexcitationsignal
		     (* loop-gain
			(delay0
			 string3-delay
			 (one-pole-allpass
			  string3-tuning-ap
			  (one-pole-allpass
			   string3-stiffness-ap1
			   (one-pole-allpass
			    string3-stiffness-ap2
			    (one-pole-allpass
			     string3-stiffness-ap3
			    (one-pole-allpass
			     string3-stiffness-ap4
			    (one-pole-allpass
			     string3-stiffness-ap5
			    (one-pole-allpass
			     string3-stiffness-ap6
			    (one-pole-allpass
			     string3-stiffness-ap7
			    (one-pole-allpass
			     string3-stiffness-ap8
			     (+ couplingfilter-output
				string3-junction-input)))))))))))))
		
		  couplingfilter-input
		  (+ string1-junction-input
		     string2-junction-input
		     string3-junction-input)
		
		  couplingfilter-output
		  (one-pole-one-zero
		   couplingfilter
		   couplingfilter-input))
;;;		 (locsig loc i couplingfilter-input)
		 (outa i couplingfilter-input)
		 (incf sampcount)))
	       )))))))))



#|

(with-sound (:output "thnormal.snd")
  (loop for i from 0 to 7 do
    (p
     (* i .5)
     :duration .5
     :keynum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :drypedalresonancefactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
  )))

(with-sound (:output "thstiff.snd")
  (loop for i from 0 to 7 do
    (p
     (* i .5)
     :duration .5
     :keynum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :drypedalresonancefactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
     
     ;;modification to do detunedness
     :detuningfactor-table '(24 5 36 7.0 48 7.5 60 12.0 72 20
				84 30 96 100 108 300)
					;scales the above detuning values
					;  so 1.0 is nominal detuning
					;  0.0 is exactly in tune (no two stage decay...)
					;  > 1.0 is out of tune...
     
     ;;modification to do stiffness
     :stiffnessfactor-table '(21 1.5 24 1.5 36 1.5 48 1.5 60 1.4
				 72 1.3 84 1.2 96 1.0 108 1.0)
					;0.0 to 1.0 is less stiff, 1.0 to 2.0 is more stiff...
     )))

(with-sound (:output "thdamped.snd")
  (loop for i from 0 to 7 do
    (p
     (* i .5)
     :duration .5
     :keynum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :drypedalresonancefactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
     
     ;;modifications to do damped sounds
     :singlestringdecayrate-table '(21 -5 24.000 -5.000 36.000 -5.4
				       41.953 -5.867 48.173 -7.113 53.818 -8.016
				       59.693 -8.875 66.605 -9.434 73.056 -10.035
				       78.931 -10.293 84.000 -12.185)
     :singlestringpole-table '(21 .8 24 0.7  36.000 .6 48 .5 60 .3
				  84 .1 96 .03 108 .03)
     :stiffnesscoefficient-table '(21.000 -0.920 24.000 -0.900 36.000 -0.700
					  48.000 -0.250 60.000 -0.100 75.179 -0.040
					  82.986 -0.040 92.240 .3 96.000 .5
					  99.000 .7 108.000 .7)
					;these are the actual allpass coefficients modified here
					;to allow dampedness at hig freqs
     )))

(with-sound (:output "thextend.snd")
  (loop for i from 5 to 5 do
    (p
     0
     :duration 10
     :keynum (+ 24 (* 12 i))
     :strike-velocity .5
					;0 to 1, 0 is softest played note, 1 is loud note
     :amp .4
					;overall volume level
     :drypedalresonancefactor .25
					;0 no open string resonance
					;1.0 is about full resonance of dampers raised
					;can be greater than 1.0
     
     ;;modification for long duration notes
     :singlestringdecayratefactor 1/10
					;scales attenuation rate (1/2 means twice as long duration)
     )))


|#
