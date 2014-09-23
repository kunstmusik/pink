(ns pink.filters
  (:require [pink.config :refer [*sr* *buffer-size*]]
            [pink.util :refer :all]))

(defn tone 
  "A first-order recursive low-pass filter with variable frequency response. (based on Csound's tone opcode). 
  
  For further information, see: http://csound.github.io/docs/manual/tone.html"
  [afn cutoff]
  (let [TPIDSR (/ (* 2 Math/PI) *sr*)
        cutoff-fn (arg cutoff)
        out ^doubles (create-buffer)
        last-val (double-array 1 0.0)]
    (fn []
      (let [in ^doubles (afn)
            hps ^doubles (cutoff-fn)] 
        (when (and in hps)
          (loop [i 0 last-value (getd last-val)]
            (if (< i *buffer-size*)
              (let [hp (aget hps i)
                    b (- 2.0 (Math/cos (* hp TPIDSR)))
                    c2 (- b (Math/sqrt (- (* b b) 1.0)))
                    c1 (- 1.0 c2)
                    new-val (+ (* c1 (aget in i)) 
                               (* c2 last-value))]
                (aset out i new-val) 
                (recur (unchecked-inc-int i) new-val))
              (aset last-val 0 last-value)))
          out)))))

(defn atone 
  "A hi-pass filter whose transfer functions are the complements of the tone function (based on Csound's atone opcode). 
  
  For further information, see: http://csound.github.io/docs/manual/atone.html"
  [afn cutoff]
  (let [TPIDSR (/ (* 2 Math/PI) *sr*)
        cutoff-fn (arg cutoff)
        out ^doubles (create-buffer)
        last-val (double-array 1 0.0)]
    (fn []
      (let [in ^doubles (afn)
            hps ^doubles (cutoff-fn)] 
        (when (and in hps)
          (loop [i 0 last-value (getd last-val)]
            (if (< i *buffer-size*)
              (let [hp (aget hps i)
                    b (- 2.0 (Math/cos (* hp TPIDSR)))
                    c2 (- b (Math/sqrt (- (* b b) 1.0)))
                    sig (aget in i)
                    new-val (* c2 (+ last-value sig))]
                (aset out i new-val) 
                (recur (unchecked-inc-int i) (- new-val sig)))
              (aset last-val 0 last-value)))
          out)))))

(defn port
  [afn half-time] 
  "Apply portamento to step-wise signal via low-pass filtering."
  (let [out ^doubles (create-buffer) 
        onedsr (/ 1.0 *sr*)
        c2 (Math/pow 0.5 (/ onedsr half-time))
        c1 (- 1.0 c2)
        last-val ^doubles (double-array 1 0.0)]
    (fn []
      (when-let [buf ^doubles (afn)] 
        (loop [i 0 previous (aget last-val 0)] 
          (if (< i *buffer-size*)
            (do 
              (let [new-val (+ (* c1 (aget buf i)) (* c2 previous))]
                (aset out i new-val)
                (recur (unchecked-inc i) new-val)))
            (do
              (aset last-val 0 previous)
              out)))))))

