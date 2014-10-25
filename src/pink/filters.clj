(defn moogladder
  [afn cutoff resonance]
  (let [out ^doubles (create-buffer)
        cfn (arg cutoff)
        rfn (arg resonance)
        sr (long *sr*)]
   (generator
     [del0 0.0 del1 0.0 del2 0.0 del3 0.0 del4 0.0 del5 0.0
      tanhstg0 0.0 tanhstg1 0.0 tanhstg2 0.0
      old-freq 0.0
      old-res -1.0
      old-acr 0.0
      old-tune 0.0]
     [asig afn
      cut cfn
      res rfn]
     (if (or (not= old-freq cut) (not= old-res res))
       (let [fc (/ cut sr)
             f (* 0.5 fc)
             fc2 (* fc fc)
             fc3 (* fc2 fc)
             fcr (+ (* 1.8730 fc3) (* 0.4955 fc2)
                    (* -0.6490 fc) 0.9988)
             acr (+ (* -3.9364 fc2) (* 1.8409 fc) 0.9968)
             tune (/ (- 1.0 (Math/exp (- (* TWO_PI f fcr)))) THERMAL)
             res4 (* 4.0 res acr)]
         (let [
               input (- asig (* res4 del5))
               stg0 (+ del0 (* tune (- (Math/tanh (* input THERMAL)) 
                                       tanhstg0)))
               new-tanhstg0 (Math/tanh (* stg0 THERMAL))
               stg1 (+ del1 (* tune (- new-tanhstg0 tanhstg1)))
               new-tanhstg1 (Math/tanh (* stg1 THERMAL))
               stg2 (+ del2 (* tune (- new-tanhstg1 tanhstg2)))
               new-tanhstg2 (Math/tanh (* stg2 THERMAL))
               stg3 (+ del3 (* tune (- new-tanhstg2 
                                       (Math/tanh (* del3 THERMAL)))))
               new-del5 (* 0.5 (+ stg3 del4))
               new-del4 stg3

               ;; second pass of 2x oversampling
               _input (- asig (* res4 new-del5))
               _stg0 (+ stg0 (* tune (- (Math/tanh (* _input THERMAL)) 
                                        new-tanhstg0)))
               _new-tanhstg0 (Math/tanh (* _stg0 THERMAL))
               _stg1 (+ stg1 (* tune (- _new-tanhstg0 tanhstg1)))
               _new-tanhstg1 (Math/tanh (* _stg1 THERMAL))
               _stg2 (+ stg2 (* tune (- _new-tanhstg1 tanhstg2)))
               _new-tanhstg2 (Math/tanh (* _stg2 THERMAL))
               _stg3 (+ stg3 (* tune (- _new-tanhstg2 
                                        (Math/tanh (* stg3 THERMAL)))))
               _new-del5 (* 0.5 (+ _stg3 new-del4))
               _new-del4 _stg3]

           (aset out indx _new-del5)
           (recur (unchecked-inc-int indx) 
                  _stg0 _stg1 _stg3 _stg3 _new-del4 _new-del5 
                  _new-tanhstg0 _new-tanhstg1 _new-tanhstg2
                  cut res acr tune))) 
       (let [acr old-acr 
             tune old-tune 
             res4 (* 4.0 res acr)]
         (let [input (- asig (* res4 del5))
               stg0 (+ del0 (* tune (- (Math/tanh (* input THERMAL)) 
                                       tanhstg0)))
               new-tanhstg0 (Math/tanh (* stg0 THERMAL))
               stg1 (+ del1 (* tune (- new-tanhstg0 tanhstg1)))
               new-tanhstg1 (Math/tanh (* stg1 THERMAL))
               stg2 (+ del2 (* tune (- new-tanhstg1 tanhstg2)))
               new-tanhstg2 (Math/tanh (* stg2 THERMAL))
               stg3 (+ del3 (* tune (- new-tanhstg2 
                                       (Math/tanh (* del3 THERMAL)))))
               new-del5 (* 0.5 (+ stg3 del4))
               new-del4 stg3

               ;; second pass of 2x oversampling
               _input (- asig (* res4 new-del5))
               _stg0 (+ stg0 (* tune (- (Math/tanh (* _input THERMAL)) 
                                        new-tanhstg0)))
               _new-tanhstg0 (Math/tanh (* _stg0 THERMAL))
               _stg1 (+ stg1 (* tune (- _new-tanhstg0 tanhstg1)))
               _new-tanhstg1 (Math/tanh (* _stg1 THERMAL))
               _stg2 (+ stg2 (* tune (- _new-tanhstg1 tanhstg2)))
               _new-tanhstg2 (Math/tanh (* _stg2 THERMAL))
               _stg3 (+ stg3 (* tune (- _new-tanhstg2 
                                        (Math/tanh (* stg3 THERMAL)))))
               _new-del5 (* 0.5 (+ _stg3 new-del4))
               _new-del4 _stg3]

           (aset out indx _new-del5)
           (recur (unchecked-inc-int indx) 
                  _stg0 _stg1 _stg3 _stg3 _new-del4 _new-del5 
                  _new-tanhstg0 _new-tanhstg1 _new-tanhstg2
                  cut res acr tune)))
       )
     
     (yield out)
     ) 
    ))
