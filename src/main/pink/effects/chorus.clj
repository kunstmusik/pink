(ns 
  ^{:doc "Chorus Effects"
    :author "Steven Yi"}
  pink.effects.chorus
  (:require [pink.config :refer :all]
            [pink.oscillators :refer :all]
            [pink.delays :refer :all]
            [pink.util :refer :all]))

(def ^:private ^:const ^{:tag 'double} LEVEL 0.3)
(def ^:private ^:const ^{:tag 'double} DELAY-MS 0.01)
(def ^:private ^:const ^{:tag 'double} DEPTH-MS 0.002)
(def ^:private ^:const ^{:tag 'double} LFO-MIN 0.5)
(def ^:private ^:const ^{:tag 'double} LFO-MAX 0.25)
(def ^:private SINE-TABLE (pink.gen/gen-sine))

(def ^:private PHASES 
  [0.00 0.08 0.17 0.25 0.33 0.42
   0.50 0.58 0.67 0.75 0.83 0.92])

(defn- chorus-line
  [^double phs]
  (oscil DEPTH-MS (+ (double (rand LFO-MAX)) LFO-MIN) SINE-TABLE phs))

(defn chorus
  "Adds stereo chorus to a stereo-signal audio-function. 
  
  Based on a Csound-coded design by Atte Andre Jenson"
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

