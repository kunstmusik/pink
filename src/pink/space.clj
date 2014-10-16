(ns pink.space
  "Functions for processing spatial qualities of sound"
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffer arg generator]]
            [pink.dynamics :refer [db->amp]]))

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
        PI2 (/ Math/PI 2)
        ]
    (generator 
      []
      [ain afn
       loc locfn]
       (let [cur-loc (+ 0.5 (* 0.5 loc))
                    ^double l (db->amp (* 20 (Math/log (Math/cos (* PI2 cur-loc )))))
                    ^double r (db->amp (* 20 (Math/log (Math/sin (* PI2 cur-loc )))))]
                (aset left indx (* l ain)) 
                (aset right indx (* r ain)) 
                (recur (unchecked-inc indx)))
      (yield out))))
