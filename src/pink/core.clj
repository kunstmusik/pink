(ns pink.core)



(defmacro afunc [state-bindings & body]
  `(let [~@state-bindings]
     (fn []
       ~@body)))

(defn box-val [v]
  (into-array (type v) [v]))

(defn create-state 
  [bindings]
  {:pre (even? bindings)}
  (reduce 
    (fn [a [b c]]
      (if (and (list? c) (= (first c) 'default))
        a 
        (conj a b (list `box-val c)))) 
    []
    (partition 2 bindings)))

(defn create-load [bindings]
  {:pre (even? bindings)}
  (reduce 
    (fn [a [b c]]
      (if (and (list? c) (= (first c) 'default))
        (conj (conj a b) (second c)) 
        (conj a b `(let [temp# (aget ~b 0)]
                    (with-meta ~b {:tag (type ~b)}) )))) 
    []
    (partition 2 bindings)))

(defn handle-yield [bindings ret-sym]
  {:pre (even? bindings)}
  `(do 
     ~@(reduce 
         (fn [a [b c]]
           (if (and (list? c) (= (first c) 'default))
             a 
             (conj a b `(let [temp# (aget ~b 0)]
                          (with-meta ~b {:tag (type ~b)}) )))) 
         []
         (partition 2 bindings))
     ~ret-sym))

(defn process-body [bindings body]
  (reverse 
    (reduce  
    (fn [a b] 
      (if (list? b)
        (if (= (first b) 'yield) 
          (cons (handle-yield bindings (second b)) a)
          (cons (process-body bindings b) a)) 
        (cons b a))) '() body)))

(defmacro generator 
  [bindings & body]
  (let [state (create-state bindings)
        new-bindings (create-load bindings)
        save-statement (create-save bindings)
        new-body (process-body bindings body)]
   `(let [~@state] 
      (fn [] 
        (loop [~@new-bindings]
          ~@new-body           
        ))) 
    ))

(let [out 4] 
  (generator [i (default 0) a 4 b (+ 3 4)]
  (if (< i 32)
    (recur (unchecked-inc i) a b)
    (yield out))))

;(create-state 0.0 0.02 "test")

;(defn test-gen []
;  (generator [phase 0.0 incr 0.02]
;    (initial-forms)  
;    (loop [i 0]
       
;      )      

;           ))
