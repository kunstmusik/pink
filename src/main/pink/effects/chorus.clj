(ns 
  ^{:doc "Chorus Effects"
    :author "Steven Yi"}
  pink.effects.chorus
  (:require [pink.config :refer :all]
            [pink.oscillators :refer :all]
            [pink.delays :refer :all]
            [pink.util :refer :all]))

(def ^:private ^{:tag 'double} LEVEL 0.3)
(def ^:private ^{:tag 'double} DELAY-MS 0.01)
(def ^:private ^{:tag 'double} DEPTH-MS 0.002)
(def ^:private ^{:tag 'double} LFO-MIN 0.5)
(def ^:private ^{:tag 'double} LFO-MAX 0.25)
(def ^:private SINE-TABLE (pink.gen/gen-sine))

;(defn- create-write-ptr
  ;[^doubles delay-buffer]
  ;(let [out (create-buffer)
        ;buffer-len (alength delay-buffer)]
    ;(generator
      ;[phs (long 0)]
      ;[]
      ;(let [new-phs (inc phs)]
        ;(aset out int-indx phs)
        ;(gen-recur (if (>= new-phs buffer-len) 0 new-phs)))
      ;(yield out))))

(def ^:private PHASES 
  [0.00 0.08 0.17 0.25 0.33 0.42
   0.50 0.58 0.67 0.75 0.83 0.92])

(defn- chorus-line
  [^double phs]
  (oscil DEPTH-MS (+ (double (rand LFO-MAX)) LFO-MIN) SINE-TABLE phs))

;; for each sample, read delay value by delay-time of lfo

(defn chorus
  "Adds stereo chorus to a stereo-signal audio-function."
  [afn ^double wet-dry]
  (let [out ^"[[D" (create-buffers 2)
        left ^doubles (aget out 0)
        right ^doubles (aget out 1)
        ^doubles delay-buffer (create-delay (+ DELAY-MS DEPTH-MS 0.1))
        buffer-len (alength delay-buffer)
        ;lfos (into-array (map chorus-line PHASES))
        line0 (chorus-line 0.00)
        line1 (chorus-line 0.00)
        line2 (chorus-line 0.17)
        line3 (chorus-line 0.25)
        line4 (chorus-line 0.33)
        line5 (chorus-line 0.42)
        line6 (chorus-line 0.50)
        line7 (chorus-line 0.58)
        line8 (chorus-line 0.67)
        line9 (chorus-line 0.75)
        line10 (chorus-line 0.83)
        line11 (chorus-line 0.92)
        dry (- 1.0 wet-dry)]
    (with-signals [[inL inR] afn] 
      (generator 
      [write-ptr (long 0)]
      [l inL
       r inR
       lfo0 line0 
       lfo1 line1 
       lfo2 line2 
       lfo3 line3 
       lfo4 line4 
       lfo5 line5 
       lfo6 line6 
       lfo7 line7 
       lfo8 line8 
       lfo9 line9 
       lfo10 line10
       lfo11 line11]
      (let [sig (* LEVEL (+ (double l) (double r)))
            d0 (delay-read-samp-i delay-buffer write-ptr (+ lfo0 DELAY-MS))
            d1 (delay-read-samp-i delay-buffer write-ptr (+ lfo1 DELAY-MS))
            d2 (delay-read-samp-i delay-buffer write-ptr (+ lfo2 DELAY-MS))
            d3 (delay-read-samp-i delay-buffer write-ptr (+ lfo3 DELAY-MS))
            d4 (delay-read-samp-i delay-buffer write-ptr (+ lfo4 DELAY-MS))
            d5 (delay-read-samp-i delay-buffer write-ptr (+ lfo5 DELAY-MS))
            d6 (delay-read-samp-i delay-buffer write-ptr (+ lfo6 DELAY-MS))
            d7 (delay-read-samp-i delay-buffer write-ptr (+ lfo7 DELAY-MS))
            d8 (delay-read-samp-i delay-buffer write-ptr (+ lfo8 DELAY-MS))
            d9 (delay-read-samp-i delay-buffer write-ptr (+ lfo9 DELAY-MS))
            d10 (delay-read-samp-i delay-buffer write-ptr (+ lfo10 DELAY-MS))
            d11 (delay-read-samp-i delay-buffer write-ptr (+ lfo11 DELAY-MS))
            chorusL (+ d0 d1 d2 d3 d4 d5)
            chorusR (+ d6 d7 d8 d9 d10 d11)
            outL (+ (* (double l) dry) (* chorusL wet-dry))
            outR (+ (* (double r) dry) (* chorusR wet-dry))
            new-write-ptr (inc write-ptr)]
        (aset left int-indx outL)
        (aset right int-indx outR)
        (aset delay-buffer write-ptr sig) 
        (gen-recur (if (>= new-write-ptr buffer-len) 0 new-write-ptr)))
      (yield out)))
    ))

;(println (disassemble (chorus #() 0.5)))

;isin    ftgentmp  0, 0, 65537, 10, 1

;ilevl   =   0.3   ; Output level
;idelay    =   0.01    ; Delay in ms
;idpth   =   0.002   ; Depth in ms
;imax    =   0.25    ; Maximum LFO rate
;imin    =   0.5   ; Minimum LFO rate
;iwave   =   isin    ; LFO waveform

;kwet    =   <wetDry>

;ain             =               ain1 + ain2 * .5

;ain             =               ain * ilevl
;i01             =               rnd (imax)
;i02             =               rnd (imax)
;i03             =               rnd (imax)
;i04             =               rnd (imax)
;i05             =               rnd (imax)
;i06             =               rnd (imax)
;i07             =               rnd (imax)
;i08             =               rnd (imax)
;i09             =               rnd (imax)
;i10             =               rnd (imax)
;i11             =               rnd (imax)
;i12             =               rnd (imax)
;alfo01          oscil           idpth, i01 + imin, iwave
;alfo02          oscil           idpth, i02 + imin, iwave, .08
;alfo03          oscil           idpth, i03 + imin, iwave, .17
;alfo04          oscil           idpth, i04 + imin, iwave, .25
;alfo05          oscil           idpth, i05 + imin, iwave, .33
;alfo06          oscil           idpth, i06 + imin, iwave, .42
;alfo07          oscil           idpth, i07 + imin, iwave, .50
;alfo08          oscil           idpth, i08 + imin, iwave, .58
;alfo09          oscil           idpth, i09 + imin, iwave, .67
;alfo10          oscil           idpth, i10 + imin, iwave, .75
;alfo11          oscil           idpth, i11 + imin, iwave, .83
;alfo12          oscil           idpth, i12 + imin, iwave, .92
;atemp           delayr          idelay + idpth +.1
;a01             deltapi         idelay + alfo01
;a02             deltapi         idelay + alfo02
;a03             deltapi         idelay + alfo03
;a04             deltapi         idelay + alfo04
;a05             deltapi         idelay + alfo05
;a06             deltapi         idelay + alfo06
;a07             deltapi         idelay + alfo07
;a08             deltapi         idelay + alfo08
;a09             deltapi         idelay + alfo09
;a10             deltapi         idelay + alfo10
;a11             deltapi         idelay + alfo11
;a12             deltapi         idelay + alfo12
;                delayw          ain
;achorusl        sum   a01, a02, a03, a04, a05, a06
;achorusr        sum             a07, a08, a09, a10, a11, a12

;aout1   =   ain1 *  (1-kwet) + achorusl * kwet
;aout2   =   ain2 *  (1-kwet) + achorusr * kwet

