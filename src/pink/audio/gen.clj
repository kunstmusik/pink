(ns pink.audio.gen
  "Table generator Functions"
  )

(defn gen-sine
  "Generates a sine wave with n size table (default to 2^16 (65536))"
  ([] (gen-sine 65536))
  ([n]
   (let [buffer (double-array n)]
     (loop [indx 0]
       (when (< indx n)
         (aset buffer indx (Math/sin 
                             (* 2.0 Math/PI (/ indx n))))
         (recur (inc indx))))
     buffer)))
