(ns pink.plotting
  (:require [pink.util :refer [shared arg mul with-duration create-buffer generator]]
            [pink.envelopes :refer [env]]
            [pink.config :refer  [*buffer-size* *current-buffer-num* *sr*]]
            [clojure.pprint :refer  [pprint]]) 
  (:use [incanter core charts]))

(defn- not-nil? [a] (not (nil? a)))

(defn- get-buffers [afns i]
  (binding [*current-buffer-num* i] 
    (let [bufs (map (fn [a] (a)) afns)]
      (if (every? not-nil? bufs)
        bufs
        nil))))

(defn- data-for-afns
  "Generates audio signals from afns. Uses doall to force updating of vector data
  as audio functions reuse buffers and data is only valid for current *current-buffer-num*."
  [afns] 
  (loop [i 0 
         ys (map (fn [a] []) (range (count afns)))]
    (let [bufs (get-buffers afns i)]
      (if (and bufs (< i 20000)) 
        (recur (unchecked-inc-int i) 
               (doall (map #(into %1 %2) ys bufs )))
        (do
          (map #(vector (range (* i *buffer-size*)) %) ys))))))

(defn visualize
  [& afns]
  (let [data (data-for-afns afns)] 
    (loop [[afn-data & rst] data 
           plt nil]
      (if afn-data
        (let [[x y] afn-data] 
          (if plt 
            (do
              (add-lines plt x y
                         :x-label "Samples"
                         :y-label "Signal"   
                         )
              (recur rst plt))
            (recur rst (xy-plot x y 
                                :x-label "Samples"
                                :y-label "Signal"))))    
        (view plt)))))

(defn visualize-with-duration 
  [dur & afns] 
  (let [args (map #(mul (env [0.0 1.0 dur 1.0]) %) afns)]
    (apply visualize args)))

;(visualize (with-duration 2.0 (adsr 0.02 0.05 0.9 0.5)))

;(visualize (with-duration 2.0 (adsr 0.5 0.5 0.8 0.5)))

;(visualize (mul (env [0.0 1.0 0.05 1.0]) 
;                (blit-pulse (env [0.0 220 1.0 220]) 0.5)))

;(visualize (mul (env [0.0 1.0 0.05 1.0]) 
;                (blit-pulse 220 0.5)))


;(visualize (mul (env [0.0 1.0 0.3 1.0]) 
;                (blit-triangle 440))
;           (mul (env [0.0 1.0 0.3 1.0]) 
;                (blit-square 440))
;           )


;(visualize (mul (env [0.0 1.0 0.3 1.0]) 
;                (blit-square-static-1 440 0))
;           (mul (env [0.0 1.0 0.3 1.0]) 
;                (blit-square-static-2 440 0))
;           )

;(let [amp 0.5
;      freq 220
;      env0  (shared 

;(let [amp 0.5
;      freq 220
;      env0  (shared 
;              (if  (number? amp)
;                (env  [0 0 0.02 amp 0.03  (* 0.9 amp) 0.5  (* 0.9 amp) 0.2 0.0] )
;                (arg amp)))
;      env1  (shared  (mul env0 env0))
;      env2  (shared  (mul env1 env0))
;      env3  (shared  (mul env2 env0))
;      envs  [env0 env1 env2 env3]
;      freqf  (shared  (arg freq))
;      phase 0.5
;      [adjust & tbls]  (horn-lookup freq horn-stopped-wave-tables) 
;      tbl-fns  (map oscil3 envs  (repeat freqf) tbls  (repeat phase))
;      ]
;  (visualize env0 env1) 
;  )

;(visualize (horn-stopped 0.5 220))
