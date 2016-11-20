(ns pink.effects.distortion
  "Distortion effects."
  (:require [pink.config :refer [*sr*]]
           [pink.util :refer :all])
  (:import [clojure.lang IFn$DD]))


(defn distort
  "Hyperbolic tangent distortion. Provides normalized (i.e. in range (-1,+1))
  or non-normalized processing (i.e. (-0.7616, 0.7616) for input (-1,+1)).
  Normalized processing is the default but is more expensive to use.
  Saturation defaults to 1.0; larger values will increase the distortion
  quality."

  ([afn] (distort afn 1.0))
  ([afn saturation] (distort afn saturation true))
  ([afn saturation normalized]
  (let [out (create-buffer)
        sfn (arg saturation)] 
    (if normalized
      ;; normalized version
      (generator 
        [] [sig afn, sat sfn]
        (do 
          (aset out int-indx 
                (* (/ 1.0 (Math/tanh sat))
                   (Math/tanh (* sat sig))))
          (gen-recur))
        (yield out)) 

      ;; non-normalized version
      (generator 
        [] [sig afn, sat sfn]
        (do 
          (aset out int-indx (Math/tanh (* sat sig)))
          (gen-recur))
        (yield out))
      ))))

(defn distort1
  "Modified hyperbolic tangent distortion. Provides separate shaping of
  positive and negative parts of signal (i.e., asymetric waveshaping). 
  distort1 using the following formula:

         exp(asig * (shape1 + pregain)) - exp(asig * (shape2 - pregain))
  aout = ---------------------------------------------------------------
         exp(asig * pregain)            + exp(-asig * pregain)

  ARGS 

  pre-gain - determines the amount of gain applied to the signal before 
    waveshaping. A value of 1 gives slight distortion.

  post-gain - determines the amount of gain applied to the signal after 
    waveshaping.

  shape1 - determines the shape of the positive part of the curve. A value of 0 
    gives a flat clip, small positive values give sloped shaping.

  shape2 - determines the shape of the negative part of the curve.

  (The above quoted from the Csound manual entry for distort1; see link below.)
  
  Based on Csound's distort1 opcode by Hans Mikelson. For further information,
  see:

  http://csound.github.io/docs/manual/distort1.html
  http://www.csoundjournal.com/ezine/winter1999/processing/
  http://folk.ntnu.no/oyvinbra/gdsp/Lesson4Modtanh.html
  "

  [afn pre-gain post-gain shape1 shape2]
  (let [out (create-buffer)
        prefn (arg pre-gain)
        postfn (arg post-gain)
        shape1fn (arg shape1)
        shape2fn (arg shape2)]
    (generator
      [] [sig afn, pre prefn, post postfn, shp1 shape1fn, shp2 shape2fn]
      (let [v (/ (- (Math/exp (* sig (+ shp1 pre)))
                    (Math/exp (* sig (- shp2 pre))))
                 (+ (Math/exp (* sig pre))
                    (Math/exp (* (- sig) pre))))] 
        (aset out int-indx (* v post)) 
        (gen-recur))
      (yield out))))

