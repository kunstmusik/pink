(ns pink.demo.live-code-study
  (:require [pink.simple :refer :all]
            [pink.live-code :refer :all]
            [pink.config :refer :all]
            [pink.filters :refer :all]
            [pink.envelopes :refer :all]
            [pink.util :refer :all]
            [pink.node :refer :all]
            [pink.oscillators :refer :all]
            [pink.space :refer :all]
            [pink.event :refer :all]
            [pink.noise :refer :all]
            [pink.effects.ringmod :refer :all]
            [pink.effects.reverb :refer :all]
            [pink.io.sound-file :refer :all]
            [clojure.string :refer [join]]
            ))

;; instr

;; Download Salamander from https://archive.org/details/SalamanderDrumkit and
;; place within PINK_RESOURCE_ROOT/samples
(def samples-root
  (str (System/getenv "PINK_RESOURCE_ROOT") 
  "/samples/salamanderDrumkit/OH/"))

(defn table 
  [filename]
  (load-table (str samples-root filename)))

(def samples
  { 48 (table "kick_OH_F_1.wav")
    49 (table "snare_OH_F_1.wav")
    50 (table "ride2_OH_FF_1.wav")
    51 (table "hihatClosed_OH_F_1.wav")

    44 (table "hihatClosed_OH_F_1.wav")
    45 (table "hihatClosed_OH_F_1.wav")
    46 (table "hihatClosed_OH_F_1.wav")
    47 (table "cowbell_FF_1.wav")
   })


(def bd 48)
(def snare 49)
(def ride 50)

(defn play-sample-one-shot
  ([^long keynum]
   (play-sample-one-shot keynum 1.0))
  ([^long keynum ^double amp-adj]
   (when-let [sample (samples keynum)] 
     (let [dur (sample-duration sample)]
       (add-afunc                               
         (-> 
           (sample-one-shot sample)
           (mul amp-adj)
           (pan 0.0))
         )))))

;; control

(defn play-samp [samp-num pattern indx amp]
  (when (pattern indx)
    (play-sample-one-shot samp-num amp)))

(defn sub-beat [n]
  (* (now) n))

(defn drums []
  (let [n (beat-mod (sub-beat 4) 16)
        bd-pat #{0 4 8 12 14}
        snare-pat #{2 4 12 15}]
    (play-samp bd bd-pat n 2.0)
    (play-samp snare snare-pat n 1.0)
    ;(play-samp ride (into #{} (range 0 16 2)) n 0.35)
    )
  (cause drums (next-beat 1/4)))


(def reverb (create-node :channels 2) )
(def reverb-fn
  (freeverb (node-processor reverb) 0.8 0.25))

(add-afunc reverb-fn)


(defn synth1 
  [dur freq]
  (with-duration (beats dur) 
    (let [e (shared (adsr 0.01 0.05 0.25 0.25))]
      (->
        (sum (blit-saw freq)
             (blit-square (* freq 2)) ) 
        (zdf-ladder (sum 500 (mul 2000 e)) 0.75)
        (mul e 0.5)
        (pan 0.0)))))

(defn synth2 
  [dur freq]
  (with-duration (beats dur) 
    (let [e (shared (adsr 0.01 (beats 0.5) 0.001 (beats 0.5)))]
      (->
        (sum (blit-saw freq)
             (blit-saw (* freq 1.0013)) ) 
        (zdf-ladder (sum 500 (mul 
                               ;(of-range (/ (beat-mod 16) 16.0) 1000 8000) 
                               3000
                                  e)) 
                    0.15)
        (mul e 0.75)
        (pan 0.0)))))

(comment
  (add-wet-dry 
    0.2  
    (-> (sum (blit-saw 400)
             (mul 0.5 (blit-saw 800)
                  (blit-saw 800.2317)))
        (zdf-ladder (sum 100 (mul 10000 (adsr 0.0 4.0 0 4.0))) 0.8)
        (mul 0.8)
        (pan 0.0)
        ))
  )


(defn add-wet-dry
  [wet-dry afn]
  (let [s (shared afn)]
    (add-afunc 
      (apply-stereo mul s (- 1.0 wet-dry)))
    (node-add-func 
      reverb 
      (apply-stereo mul s wet-dry)))) 

(defn m1
  [indx]
  (let [dur (rand-nth [1/4 1/2 1 1])
        freq (* 100 (inc indx)) 
        wet-dry 0.5]
    (add-wet-dry wet-dry (synth1 dur freq))
    (cause m1 (next-beat dur) 
           (mod (inc indx) 8)
           )))

(defn m2 [pchs durs]
  (let [p (next-in-atom pchs)
        d (next-in-atom durs)
        wet-dry 0.1] 
    (add-wet-dry wet-dry (synth2 d p))
    (cause m2 (next-beat d) (!r! pchs) (!r! durs))))

(defn of-range [^double n ^double min-val ^double max-val]
  (+ min-val (* n (- max-val min-val))))

(defn m3 []
  (let [n (beat-mod (sub-beat 4) 16)
        pat #{0 2 6}
        wet-dry 0.2]
    (when (pat n)
      (add-wet-dry wet-dry (synth1 1/4 (of-range (/ (inc n) 16.0) 600 700)))))
  (cause m3 (next-beat 1/4)))

(defn m4 []
  (let [n (beat-mod (sub-beat 4) 16)
        pat #{0 1 2 3 4 5 6 7}
        wet-dry 0.3]
    (when (pat n)
      (add-wet-dry wet-dry (synth2 1/2 80))))
  (cause m4 (next-beat 1/4)))


(defn m5-freq [] 80)
(defn m5 []
  (let [n (beat-mod (sub-beat 4) 16)
        pat #{0 1 2 3 4 5 6 7}
        wet-dry 0.1]
    (when (pat n)
      (add-wet-dry wet-dry (synth2 1/2 (m5-freq)))))
  (cause m5 (next-beat 1/4)))

(def m6-pchs 
  (atom 
    (cycle (concat 
      (repeat 16 160)
      (repeat 16 200)
      (repeat 16 300)
      (repeat 16 400)
      ))))

(defn m6-freq []
  (next-in-atom m6-pchs))

(defn m6 []
  (let [n (beat-mod (sub-beat 4) 16)]
    (add-wet-dry 0.05 (synth1 (beats 1/4) (m6-freq)))) 
  (cause m6 (next-beat 1/4)))

(comment
    
  (start-engine)

  (set-tempo 106)

  ;; eval to get drums going
  (cause drums (next-beat 4))

  ;; eval to get melodic line going
  ;; eval multiple times to get parallel melodic lines 
  (cause m1 (next-beat 4) 0)

  ;; can mutate the pattern sequences while m2 is running in its event stream
  (def m2-pchs (atom nil))
  (reset!! m2-pchs (cycle [60 120 60]))

  (def m2-durs (atom nil))
  (reset!! m2-durs (repeatedly #(rand-nth [1/2 1 1/2])))
  
  (cause m2 (next-beat 4) (!r! m2-pchs) (!r! m2-durs))

  ;; sequence ahead 64-beats of changes
  (let [t (next-beat 16)]
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1]))))
           t)
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1/4]))))
           (+ t 16))
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1]))))
           (+ t 32))
    (cause (fn [] (reset!! m2-durs (repeatedly #(rand-nth [1/2 1/4]))))
           (+ t 48))
    (cause (fn [] (end-recur! m2)) 
           (+ t 64)))

  (cause m3 (next-beat 4))
  (cause m4 (next-beat 4))

  (cause m5 (next-beat 4))
  (redef! m5-freq 
          (fn [] 
            (if (> (Math/random) 0.85)
              (* 80 4) 80)))

  ;; modify m5-freq to yield different values
  (redef! m5-freq 
          (let [pat (atom (cycle [80 90 100 200]))]
          (fn [] 
            (next-in-atom pat))))

  ;; schedule the function change for m5-freq
  (cause (next-beat 4)
       (redef! m5-freq 
          (let [pat (atom (cycle [80 90 100 200]))]
          (fn [] 
            (next-in-atom pat)))))

  ;; eval to show beat/bar structure in REPL
  (cause beat-printer (next-beat 4) 4 16)

  (end-recur! drums)
  (end-recur! m2)

  (cause m6 (next-beat 4))

  (add-afunc 
    (with-duration (beats 16) 
      (let-s [e (adsr (beats 8) (beats 8) 0.0 0.0)] 
        (->
          (sum (blit-triangle 300) (blit-triangle 600))
          (zdf-ladder (sum 300 (mul e 4000)) 0.25)
          (mul 0.5 e )
          (pan 0.1)
          ))))



  (stop-engine)
  
  )

