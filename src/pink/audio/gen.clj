(ns pink.audio.gen
  "Table generator Functions"
  )

(def TWO_PI (* 2.0 Math/PI))

(defmacro get-sine-value
  [phase]
  `(Math/sin (* TWO_PI ~phase)))

(defn gen-sine
  "Generates a sine wave with n size table (default to 2^16 (65536))"
  ([] (gen-sine 65536))
  ([n]
   (let [buffer (double-array n)]
     (loop [indx 0]
       (when (< indx n)
         (aset buffer indx (get-sine-value (/ indx n)))
         (recur (inc indx))))
     buffer)))

(defn gen10
  "Generates a set of sine waves, given a list of pairs of partial and
  amplitude values"
  [tbl-size & pts]
  {:pre (even? (count pts))}
  (let [pairs (partition 2 pts)
        out (double-array tbl-size)]
      (loop [[[harmonic strength] & xs] pairs]
        (if (and harmonic strength)
          (do 
            (println "Harmonic: " harmonic "Strength: " strength)
            (loop [indx 0]
            (when (< indx tbl-size) 
              (aset out indx 
                  (+ (aget out indx)
                    (get-sine-value 
                      (* strength (rem (* harmonic (/ indx tbl-size)) 1)))))
              (recur (unchecked-inc-int indx))))
            (recur xs)) 
          out))))
