(ns pink.filters
  (:require [pink.config :refer [*sr* *buffer-size*]]
            [pink.util :refer :all]))

(defn tone 
  "A first-order recursive low-pass filter with variable frequency response. (based on Csound's tone opcode). 
  
  For further information, see: http://csound.github.io/docs/manual/tone.html"
  [afn cutoff]
  (let [TPIDSR (/ (* 2 Math/PI) *sr*)
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
                (recur (unchecked-inc indx) new-val))
      (yield out))))

(defn atone 
  "A hi-pass filter whose transfer functions are the complements of the tone function (based on Csound's atone opcode). 
  
  For further information, see: http://csound.github.io/docs/manual/atone.html"
  [afn cutoff]
  (let [TPIDSR (/ (* 2 Math/PI) *sr*)
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
                (recur (unchecked-inc indx) (- new-val ain)))
      (yield out))))

(defn port
  [afn half-time] 
  "Apply portamento to step-wise signal via low-pass filtering."
  (let [out ^doubles (create-buffer) 
        onedsr (/ 1.0 *sr*)
        c2 (Math/pow 0.5 (/ onedsr half-time))
        c1 (- 1.0 c2)
        last-val ^doubles (double-array 1 0.0)]
    (generator
      [last-val 0.0]
      [ain afn]
       (let [new-val (+ (* c1 ain) (* c2 last-val))]
                (aset out indx new-val)
                (recur (unchecked-inc indx) new-val))
      (yield out))))

