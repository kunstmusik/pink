(ns audio-seq.core
  (:use [audio-seq.engine :as engine])
  (:use audio-seq.util))

(def ^:const PI Math/PI)

(defn ^double dec-if [^double a] (if (> a 1) (dec a) a))

(defn phasor2 [^double freq ^double phase]
  (let [phase-incr ^double (/ freq  *sr*)
        cur-phase (double-array 1 phase)
        out (create-buffer)]
      (fn ^doubles [] 
        (fill out cur-phase #(dec-if (+ phase-incr ^double %))))))

(defn sinev [^double freq ^double phase]
  (let [phasor (phasor2 freq phase)
        out (create-buffer)]
    (fn ^doubles []
      (map-d #(Math/sin (* 2.0 PI ^double %)) (phasor) out))))

(defn ^doubles mul-d [^doubles a ^doubles b ^doubles out]
   (map-d #(* ^double %1 ^double %2) a b out))

(defn mul [a b]
  (let [out (create-buffer)]
    (fn ^doubles []
      (map-d #(* ^double %1 ^double %2) ^doubles (a) ^doubles (b) out))))

(defn const [^double a]
  (let [out (create-buffer a)]
  (fn ^doubles []
    out)))

(defn mix
  [& args]
    (if (> (count args) 1)
      (let [tmp (create-buffer)
            out (create-buffer)
          adjust (create-buffer (/ 1.0 (count args)))]
        (fn ^doubles []
          (mul-d adjust (reduce-d #(+ ^double %1 ^double %2) tmp args) out)))
      (nth args 0)))

(defn make-env-data [pts]
  {:pre (even? (count pts))}
  (let [[x & xs] (partition 2 pts)]
    (second (reduce (fn [[[a b] lst] [c d :as p]] 
              (let [run (double (* c *sr*))
                   rise (double (/ (- d b) run))] 
             [p (conj lst [run rise])] ))
                         [x []] xs))))

(defn env-get-inc [data counter]
  (loop [cnt 0.0 [x & xs] data]
    (if x
      (let [[a b] x
            c (+ cnt a)]
        (if (< counter c)
          b
          (recur c xs))) 
      0.0)))


(defn env [pts]
 {:pre (even? (count pts))}
  (let [linedata (make-env-data pts)
        cur-val (double-array 1 (nth pts 0))
        counter (long-array 1 -1)
        out (create-buffer)]
  (fn ^doubles[]
    (fill out cur-val #(+ ^double % ^double (env-get-inc linedata (swapl! counter inc)))))))
    
(defn audio-block3 [x]
  (mul
    (apply mix 
     (map #(sinev (* % 60) 0)
        (take x (iterate inc 1))))
    (env [0.0 0.0 0.05 1 0.05 0.9 0.5 0.9 0.5 0])))

(defn demo3 [x] (engine/run-audio-block (audio-block3 x)))

