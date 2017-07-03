(ns pink.demo.cache
  (:require [pink.engine :refer :all]
            [pink.config :refer :all]
            [pink.envelopes :refer [adsr]]
            [pink.oscillators :refer [sine]]
            [pink.util :refer :all]
            [pink.simple :refer [i]]
            [pink.space :refer :all]
            ))

(defn i1
  [freq] 
  (mul (adsr 0 1 0 1)
       (sine freq)))

(defn play-from-cache
  [^doubles s]
  (let [^doubles out (create-buffer)
        s-size (alength s)]
    (println "SSIZE: " s-size)
    (generator
      [read-ptr 0] [] 
      (if (and (= 0 int-indx) (>= read-ptr s-size)) 
        nil
        (if (< read-ptr s-size) 
          (do 
            (aset out int-indx (aget s read-ptr))
            (gen-recur (+ 1 read-ptr)))
          (do 
            (aset out int-indx 0.0)
            (gen-recur (+ 1 read-ptr)))
          ))
      (yield out))))

(defn test-engine->buffer
  []
  (let [e (engine-create :nchnls 1)]
    (->>
      (map #(apply i %)
           [[i1 0 1 440]
            [i1 1 1 550]
            [i1 2 1 660]])
      (audio-events e)
      (engine-add-events e))
    (engine->buffer e)))



(defn test-it [] 
  (let [e (engine-create :nchnls 2)
        buf (test-engine->buffer)]
    (engine-start e) 
    (engine-add-afunc e (pan (play-from-cache buf) 0.0))
    (Thread/sleep 3000.0)
    (engine-stop e)))
