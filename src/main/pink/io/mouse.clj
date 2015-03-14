(ns ^{:doc "Control-rate functions for getting the mouse's x/y location"
      :author "Steven Yi" }
  pink.io.mouse
  (:import [java.awt MouseInfo Toolkit]
           [java.util Arrays])
  (:require [pink.config :refer :all]
            [pink.util :refer [shared create-buffer]]
            [primitive-math :refer [not==]]))


(def mouse-x-val (atom 0.0))
(def mouse-y-val (atom 0.0))

(let [my-buf-num (long-array 1 -1)] 
  (defn- update-mouse-vals!
  []
  (when (not== (aget my-buf-num 0) (long *current-buffer-num*))
    (aset my-buf-num 0 (long *current-buffer-num*))
    (let [pt (.. MouseInfo getPointerInfo getLocation)]
      (reset! mouse-x-val (double (.x pt)))
      (reset! mouse-y-val (double (.y pt)))))))

(defn mouse-read-impl
  [mouse-atom]
  (let [out ^doubles (create-buffer) 
        last-val (double-array 1 @mouse-atom)]
    (Arrays/fill out ^double @mouse-atom)
    (fn []
      (update-mouse-vals!)
      (let [last-x (aget last-val 0)
            cur-x ^double @mouse-atom] 
        (if (not= last-x cur-x) 
         (do 
           (loop [i 0
                 v last-x
                 incr (/ (- cur-x last-x) (double *buffer-size*))]
            (when (< i *buffer-size*)
              (aset out i v)
              (recur (unchecked-inc i)
                     (+ v incr)
                     incr)))
          (aset last-val 0 ^double cur-x))
         (Arrays/fill out ^double cur-x)))      
      out))) 

(defn mouse-x [] 
  (mouse-read-impl mouse-x-val))

(defn mouse-y [] 
  (mouse-read-impl mouse-y-val))

(defn get-screen-width []
  (.. Toolkit getDefaultToolkit getScreenSize getWidth))

(defn get-screen-height []
  (.. Toolkit getDefaultToolkit getScreenSize getHeight))
