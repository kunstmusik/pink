(ns pink.gen
  "Table generator Functions"
  (:import [java.util Arrays]))

(def TWO_PI (* 2.0 Math/PI))

(defn get-sine-value
  [^double phase]
  (Math/sin (* ^double TWO_PI phase)))

(defn gen-sine
  "Generates a sine wave with n size table (default to 2^16 (65536))"
  ([] (gen-sine 65536))
  ([^long n]
   (let [^doubles buffer (double-array n)]
     (loop [indx (int 0)]
       (when (< indx n)
         (let [^double v (get-sine-value (/ indx (double n)))] 
           (aset buffer indx v))
         (recur (inc indx))))
     buffer)))


(defn min-max 
  "Returns the min and max values of the given table."
  [^doubles tbl]
  (let [len (alength tbl)] 
    (loop [i 0
         mn Double/MAX_VALUE
         mx Double/MIN_VALUE]
      (if (< i len) 
        (let [v (aget tbl i) 
            new-mn (if (< v mn) v mn)
            new-mx (if (> v mx) v mx)]
           (recur (unchecked-inc i) new-mn new-mx))
        [mn mx]))))

(defn table-max
  "Finds max absolute value within a table"
[^doubles tbl]
  (areduce tbl indx ret 0.0 
         (Math/max ret (Math/abs (aget tbl indx)))))

(defn rescale!
  "Rescales values in table to -1.0,1.0. Not: this is a destructive change."
  [^doubles tbl]
  (let [len (alength tbl)
        rescale-val (table-max tbl)]
    (loop [indx 0]
      (when (< indx len)
        (aset tbl indx (/ (aget tbl indx) rescale-val))
        (recur (unchecked-inc indx))))
    tbl))

; GEN routines

(defn gen9
  "Generates a set of sine waves, given a list of lists of values 
  in the form of [partial strength & phase]. The partial must be a positive 
  number, but may be fractional.  However, fractional partial values will
  generate truncated, non-full cycle waveforms. Strengths are in the range of
  0.0 to 1.0. Phases are optional and are expressed in 0-360 degrees, defaulting
  to 0."
  [tbl-size & pts]
  {:pre (every? #(pos? ^double (first %)) pts)}
  (let [size (long tbl-size)
        dbl-size (double size)
        out (double-array size)]
    (loop [[[^double harmonic ^double strength & [^double phs]] & xs] pts]
      (when (and harmonic strength)
        (let [phs-adj (if (nil? phs) 
                        0.0
                        (rem (/ phs 360.0) 1.0))] 
          (loop [indx 0]
            (when (< indx size) 
              (let [cur-val (aget out indx) 
                    ^double sine-val 
                    (get-sine-value (rem (+ phs-adj 
                                            (* harmonic (/ indx dbl-size))) 1))
                    new-val (+ cur-val (* strength sine-val))]
               (aset out indx new-val)
              (recur (unchecked-inc indx)))))
          (recur xs)))) 
    (rescale! out)))

(defn gen10
  "Generates a set of sine waves, given a list of amplitude values for each 
  harmonic"
  [tbl-size & pts]
  (let [size ^long tbl-size 
        out (double-array size)]
    (loop [harmonic 1 [^double strength & xs] pts]
      (when (some? strength)
        (if (<= strength 0)
          (recur (unchecked-inc harmonic) xs)
          (do
            (loop [indx (int 0)]
              (when (< indx size) 
                (let [last-val (aget out indx)
                      ^double sine-val 
                      (get-sine-value 
                        (* strength 
                           (rem (* harmonic (/ indx (double size))) 1)))] 
                  (aset out indx (+ last-val sine-val))
                  (recur (unchecked-inc indx)))))
            (recur (unchecked-inc harmonic) xs)))))
    (rescale! out)))

(defn gen17
  "Generates a step-wise function from x/y pairs"
  [tbl-size & pts]
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
