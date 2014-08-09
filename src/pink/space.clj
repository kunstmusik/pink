(ns pink.space
  "Functions for processing spatial qualities of sound"
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffer arg]]
            [pink.dynamics :refer [db->amp]]))

(defn pan 
  "Stereo panning using formula from MIDI GM-2 Default Pan Curve (RP-036)

  Left Channel Gain [dB] = 20*log (cos (Pi/2* max(0,CC#10 ? 1)/126))
  Right Channel Gain [dB] = 20*log (sin (Pi /2* max(0,CC#10 ? 1)/126))

  Instead of range 0-127, use [-1.0,1.0]
  "
  [afn loc]
  (let [left ^doubles (create-buffer)
        right ^doubles (create-buffer)
        locfn (arg loc)
        out (into-array [left right])
        PI2 (/ Math/PI 2)
        ]
    (fn []
      (when-let [ain ^doubles (afn)] 
        (let [locs ^doubles (locfn)] 
          (loop [i 0]
            (when (< i *ksmps*)
              (let [v (aget ain i)
                    cur-loc (+ 0.5 (* 0.5 (aget locs i)))
                    l (db->amp (* 20 (Math/log (Math/cos (* PI2 cur-loc )))))
                    r (db->amp (* 20 (Math/log (Math/sin (* PI2 cur-loc )))))]
                (aset left i (* l v)) 
                (aset right i (* r v)) 
                (recur (unchecked-inc-int i)))))
          out)))))


