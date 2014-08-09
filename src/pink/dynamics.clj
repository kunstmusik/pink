(ns pink.dynamics
  "Functions for dealing with dynamics/amplitude of audio")

(def ^:const LOG10D20
    (/  (Math/log 10) 20))

(defn db->amp
    "Convert decibel to power ratio"
      [d] 
        (Math/exp  (* d LOG10D20)))

