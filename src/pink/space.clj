(ns pink.space
  "Functions for processing spatial qualities of sound"
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffer arg generator]]
            [pink.dynamics :refer [db->amp]]
            [primitive-math :refer [not==]]
            ))

(defn pan 
  "Stereo panning using formula from MIDI GM-2 Default Pan Curve (RP-036)

  Left Channel Gain [dB] = 20*log (cos (Pi/2* max(0,CC#10 ? 1)/126))
  Right Channel Gain [dB] = 20*log (sin (Pi /2* max(0,CC#10 ? 1)/126))

  Instead of range 0-127, use [-1.0,1.0]

  If loc is an audio-function, it should be a non-ending signal generator
  otherwise on pre-mature end, the signal may zero out until the nil end
  signal is given.  This would caues the loc to snap to center during the
  last buffer generated."
  [afn loc]
  (let [left ^doubles (create-buffer)
        right ^doubles (create-buffer)
        locfn (arg loc)
        out (into-array [left right])
        PI2 (/ Math/PI 2)]
    (generator 
      [last-loc Double/NEGATIVE_INFINITY
       last-loc-v Double/NEGATIVE_INFINITY 
       last-l Double/NEGATIVE_INFINITY
       last-r Double/NEGATIVE_INFINITY]
      [ain afn
       loc locfn]
       (if (not== last-loc loc)
         (let [new-loc-v (+ 0.5 (* 0.5 loc))
               new-l (db->amp (* 20 (Math/log (Math/cos (* PI2 new-loc-v )))))
               new-r (db->amp (* 20 (Math/log (Math/sin (* PI2 new-loc-v )))))]
            (aset left int-indx (* new-l ain)) 
            (aset right int-indx (* new-r ain)) 
            (recur (unchecked-inc indx) loc new-loc-v new-l new-r))
         (do
           (aset left int-indx (* last-l ain)) 
           (aset right int-indx (* last-r ain)) 
           (recur (unchecked-inc indx) loc last-loc-v last-l last-r)))
       (yield out))))
