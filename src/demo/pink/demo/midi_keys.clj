(ns pink.demo.midi-keys
  (:require [pink.simple :refer :all] 
            [pink.io.midi :refer :all]
            [pink.config :refer :all]
            [pink.space :refer :all]
            [pink.oscillators :refer :all]
            [pink.envelopes :refer :all]
            [pink.filters :refer [port butterlp moogladder]]
            [pink.util :refer :all])
  (:import [javax.sound.midi ShortMessage ]
           [clojure.lang IFn]))


(def midim (create-midi-manager))
(def keyboard (add-virtual-device midim "keyboard 1")) 

(defn saw
  [freq amp]
  (let-s [amp-env (adsr 0.02 0.02 0.9 0.25) 
          f (sum freq (mul freq 0.0025 (sine 4)))] 
    (->
      (sum (mul 0.25 (blit-saw (mul f 2.000)))
           (blit-saw f)
           (blit-saw (mul f 0.9995))
           (sine2 (mul f 0.5)))
      (div 2.0)
      (butterlp 2000)
      (mul amp amp-env)
      (pan 0.0)
      ))) 

(comment
  ;(bind-device midim "nanoKEY KEYBOARD" "keyboard 1")
  (bind-device midim "MPKmini2" "keyboard 1")

  (bind-key-func
    keyboard 0

    (let  [allocator (create-max-allocator 8) 
           active ^"[[Z"  (make-array Boolean/TYPE 128 1)] 
      (fn  [cmd note-num velocity]
        (condp = cmd
          ShortMessage/NOTE_ON
          (when (acquire-alloc! allocator) 
            (let [done (boolean-array 1 false)
                   afn (binding [*done* done] 
                          (with-allocator allocator 
                            (saw (midi->freq note-num) 
                                    (/ (double velocity) 127.0))))]
              (aset active note-num done)
              (add-afunc afn)
              ))

          ShortMessage/NOTE_OFF
          (when-let [^booleans done (aget active note-num)]
            (aset done 0 true)
            (aset active note-num nil)))))

    )


  (start-engine)
  
  )

