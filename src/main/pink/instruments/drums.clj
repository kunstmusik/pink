(ns pink.instruments.drums
  "Drum instruments."

  (:require [pink.filters :refer :all]
            [pink.util :refer :all]
            [pink.oscillators :refer :all]
            [pink.noise :refer :all]))

(defn exp-decay [^double decay ^double length]
  (let [out (create-buffer)] 
    (generator 
      [phs (long 0)]
      []
      (do
        (if (>= phs length) 
          (aset out int-indx 0.0)
         
          (aset out int-indx (Math/pow decay (/ phs length)))) 
        (gen-recur (inc phs))) 
      (yield out))))

(defn g-noise [^double rng]
  (let [out (create-buffer)
        drng (* 2.0 rng)] 
    (generator 
    [][]
    (let [v (- (* (Math/random) drng) rng)]
      (aset out int-indx (if (> v 0.0) v 0.0))
      (gen-recur))
    (yield out))))

(defn end-when-silent
  "FIXME: This will not work when buffer-size = 1..."
  [afn]
  (fn []
    (when-let [^doubles sig (afn)]
      (if (and (zero? (aget sig 0))
         (zero? (aget sig (dec (alength sig)))))
        nil
        sig))))

(defn kick 
  "Kick Drum. 

  Ported from Charlie Roberts' Gibberish"
  ([] (kick 2.0))
  ([amp] (kick amp 50.0 20.0 1000.0))
  ([amp freq decay tone]
   (end-when-silent
     (-> 
       (pulse 0.0 60.0)  
       (zdf-2pole freq 20.0)
       (get-channel 1) ;; band-pass signal
       (zdf-2pole tone 0.5)
       (get-channel 0) ;; low-pass signal
       (mul amp)))))

(defn conga
  "Conga. 
  
  Ported from Charlie Roberts' Gibberish"
  ([] (conga 2))
  ([amp] (conga amp 190))
  ([amp freq]
   (end-when-silent
     (-> 
     (pulse 0.0 60.0)  
     (zdf-2pole freq 50.0)
     (get-channel 1)
     (mul amp)))))

(defn clave
  "Clave. 

  Ported from Charlie Roberts' Gibberish"
  ([] (clave 1.0))
  ([amp] (clave amp 2500))
  ([amp freq]
   (end-when-silent 
     (-> 
       (pulse 0.0 2.0)  
       (zdf-2pole freq 5.0)
       (get-channel 1)
       (mul amp)))))


(defn tom 
  "Tom. 

  Ported from Charlie Roberts' Gibberish"
  ([] (tom 0.5))
  ([amp] (tom amp 80))
  ([amp freq]
   (end-when-silent
     (-> 
       (sum 
         (-> 
           (pulse 0.0 60.0)  
           (zdf-2pole freq 30.0)
           (get-channel 1)
           )
         (-> 
           (g-noise 8) 
           (mul (exp-decay 0.05 11025))
           (zdf-2pole  120.0 0.5)
           (get-channel 0)))
       (mul amp)
       ))))

(defn clap
  ([] (clap 0.5))
  ([amp]
   #_(end-when-silent
     (->
          
       )
     )
   )
  )

(defn cowbell
  []
  )

(defn snare [tune cutoff snappy amp])

(defn hat
  []
  )
