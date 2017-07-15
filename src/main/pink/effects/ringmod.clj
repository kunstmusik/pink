(ns pink.effects.ringmod
  (:require [pink.util :refer [generator gen-recur create-buffer]])) 

;; Ensure unchecked math used for this namespace
(set! *unchecked-math* :warn-on-boxed)

(defn- create-ringmod-table
  ([] (create-ringmod-table 1.0))
  ([^double distortion] 
   (let [table-len (long (Math/pow 2 16))
         half (double (/ (long table-len) 2))
         table (double-array table-len) 
         vb 0.2
         vl 0.4
         h distortion 
         vl_vb2 (Math/pow (- vl vb) 2)
         vl_vb_denom (- (* 2 vl) (* 2 vb))
         vl_add (* h (/ vl_vb2 vl_vb_denom)) 
         h_vl (* h vl)]
     (loop [i 0]
       (when (< i table-len)
         (let [v (Math/abs (/ (- i half) half))] 
           (cond 
             (<= v vb)
             (aset table i 0.0)
             (<= v vl)
             (aset table i 
                   (* h (/ (Math/pow (- v vb) 2) 
                           vl_vb_denom)))
             :else
             (aset table i 
                   (+ (* h v) (- h_vl) vl_add)))
           (recur (unchecked-inc i)))))  
     table)))

;; this needs to be moved to another place, renamed
;; so that a tablei ugen can be made
(defn tablei
  ^double [^doubles table ^double indx] 
  (let [table-len (long (alength table))
        max-indx (double (- table-len 1))
        indxt (Math/min max-indx
                        (* (Math/max (Math/min indx 1.0) 0.0) table-len))
        indx0 (long indxt)
        indx1 (unchecked-inc indx0)
        frac1 (- indxt indx0)
        frac0 (- 1.0 frac1)
        v0 (aget table indx0)
        v1 (aget table (if (>= indx1 table-len) (- table-len 1) indx1))]
    (+ (* frac0 v0) (* frac1 v1))))

(defn ringmod
  "Implementation of Julian Parker's digital model of a 
  diode-based ring modulator.

  Experimental. Implementation does not currently do oversampling.  

  For more information:

  http://www.acoustics.hut.fi/publications/papers/dafx11-ringmod/
  http://recherche.ircam.fr/pub/dafx11/Papers/66_e.pdf
  http://webaudio.prototyping.bbc.co.uk/ring-modulator/"
  ([in-afn carrier-afn] (ringmod in-afn carrier-afn 1.0))
  ([in-afn carrier-afn distortion]
   (let [out (create-buffer)
         ringmod-table (create-ringmod-table distortion)] 
     (generator
       []
       [sig in-afn, carrier carrier-afn]
       (let [in (* sig 0.5)
             car (+ carrier in)
             in2 (- carrier in) 
             sig1 (tablei ringmod-table (+ 0.5 car)) 
             sig2 (tablei ringmod-table (+ 0.5 (* -1.0 car))) 
             sig3 (tablei ringmod-table (+ 0.5 in2)) 
             sig4 (tablei ringmod-table (+ 0.5 (* -1.0 in2))) 
             siginv (* -1 (+ sig3 sig4))
             v (+ sig1 sig2 siginv)]
         (aset out int-indx v)
         (gen-recur))
       (yield out)))))

