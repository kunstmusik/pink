(ns audio-seq.demo.demo3
  (:require [audio-seq.engine :as eng]
            [audio-seq.envelopes :refer [env]]
            [audio-seq.oscillators2 :refer [sine sine2]]
            [audio-seq.util :refer [mix mul const create-buffer getd setd!]]))

;TODO The below doesn't work as mul returns a function... need to sort out how to break up and share something like the result of env amongst multiple other uses
;


(comment
  (defmacro letm 
  [args & body]
  (let [parts (partition 2 args)]
    (do
      (println parts) 
      `(fn []
         ((println ~args)
          ~@body)))))

(macroexpand-1 '(letm [e "a"] 
  (println "test2")
  ))
  
  )

(defn read-buffer [buffer-ref] (fn [] @buffer-ref))
(defn write-buffer 
  [buffer-ref func] 
  (fn []
   (reset! buffer-ref (func))))

(defn split-comp 
  [func]
  (let [buffer-ref (atom create-buffer)]
   [(read-buffer buffer-ref)
    (write-buffer buffer-ref func)] ))


(comment
  
  function should return two functions, one that updates an a-buffer, another that
  reads from it
 

  )

(defn shared [afn] 
  "Wraps an audio function so that it only generates values once per ksmps block; uses 
  *curent-buffer-num* dynamic variable to track if update is required" 
  (let [my-buf-num (atom -1)
        buffer (atom nil) ]
    (fn []
      (if (not= @my-buf-num eng/*current-buffer-num*)
        (do 
          (reset! my-buf-num eng/*current-buffer-num*)
          (reset! buffer (afn))) 
        @buffer))))

(let [f (atom 0)] 
  (defn t [] (swap! f inc)))

(def q (shared t))

(def z 
  (let [cur-time (atom -1)]
  (fn [] 
    (binding [eng/*current-buffer-num* (swap! cur-time inc)]
      (* (q) (q))))))
;;(z)

(defn fm-synth [freq]
  (let [e (shared (env [0.0 0.0 0.05 2 0.02 1.5 0.2 1.5 0.2 0]))] 
    (fn [] 
      (mul
        (sine2 (mul
                 freq
                 (mul e (sine (* 1 freq)))))
        (mul 0.5 e)))))


(defn demo [e]
  (let [melody (take (* 4 8) (cycle [220 330 440 330]))
        dur 0.25]
    (loop [[x & xs] melody]
      (when x
        (let [afs (e :pending-funcs)]
          (dosync
            (alter afs conj (fm-synth x) (fm-synth (* 2 x))))
          (recur xs))))))


(defn demo-afunc [e]
  (let [melody (ref (take (* 4 8) (cycle [220 330 440 330])))
        dur 0.25 
        cur-time (double-array 1 0.0)
        time-incr (/ eng/*ksmps* 44100.0)
        afs (e :pending-funcs)
        out (create-buffer)]
    (dosync (alter afs conj (fm-synth 440)))
    (fn ^doubles []
      (let [t (+ (getd cur-time) time-incr)]
        (when (> t dur)
          (dosync (alter afs conj (fm-synth 220))))
        (setd! cur-time (rem t dur)))
      out
      )))


;;

(comment

  (defn note-sender[e]
    (dosync
      (alter (e :pending-funcs) conj (fm-synth 440) (fm-synth 660))))

  (def e (eng/engine-create))
  (eng/engine-start e)
  (eng/engine-add-afunc e (demo-afunc e))
  (eng/engine-stop e)

  (eng/engine-clear e)
  e

  (let [e (eng/engine-create)]
    (eng/engine-start e)
    (demo e)
    (Thread/sleep 500)
    (eng/engine-stop e)))


