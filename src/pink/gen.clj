(ns pink.gen
  "Table generator Functions"
  (:import [java.util Arrays])
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

(defn rescale
  [tbl]
  tbl
  )

(defn gen9
  "Generates a set of sine waves, given a list of lists of values 
  in the form of [partial strength & phase]. The partial must be a positive 
  number, but may be fractional.  However, fractional partial values will
  generate truncated, non-full cycle waveforms. Strengths are in the range of
  0.0 to 1.0. Phases are optional and are expressed in 0-360 degrees, defaulting
  to 0."
  [tbl-size & pts]
  {:pre (every? #(pos? (first %)) pts)}
  (let [out (double-array tbl-size)]
    (loop [[[harmonic strength & [phs]] & xs] pts]
      (when (and harmonic strength)
        (let [phs-adj (if (nil? phs) 
                        0.0
                        (rem (/ phs 360.0) 1.0))] 
          ;(println "Harmonic: " harmonic "Strength: " strength "Phase: " phs-adj)
          (loop [indx 0]
            (when (< indx tbl-size) 
              (aset out indx 
                    (+ (aget out indx)
                       (* strength
                         (get-sine-value   (rem (+ phs-adj 
                                             (* harmonic (/ indx tbl-size))) 1)))))
              (recur (unchecked-inc-int indx))))
          (recur xs)))) 
    out))

(defn gen10
  "Generates a set of sine waves, given a list of amplitude values for each 
  harmonic"
  [tbl-size & pts]
  (let [out (double-array tbl-size)]
    (loop [harmonic 1 [strength & xs] pts]
      (when (some? strength)
        (if (<= strength 0)
          (recur (unchecked-inc-int harmonic) xs)
          (do
            (loop [indx 0]
              (when (< indx tbl-size) 
                (aset out indx 
                      (+ (aget out indx)
                         (get-sine-value 
                           (* strength (rem (* harmonic (/ indx tbl-size)) 1)))))
                (recur (unchecked-inc-int indx))))
            (recur (unchecked-inc-int harmonic) xs)))))
    out))

(defn gen17
  "Generates a step-wise function from x/y pairs"
  [tbl-size & pts]
  ;(println pts)
  (let [pairs (partition 2 pts)
        out (double-array tbl-size)]
    (loop [[[^int x1 ^double y1] & xs] pairs]

      (let [[^int x2 ^double y2] (first xs)] 
        (if (and x2 y2)
          (do 
            (Arrays/fill out x1 x2 y1)
            (recur xs)) 
          (Arrays/fill out x1 (int tbl-size) y1)))) 
    out))
