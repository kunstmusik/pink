(ns compose.engine
  "Audio Engine Code"
  (:import 
    (java.util Arrays)))


(def engines (ref []))

(defn engine-create []
  "Creates an engine"
  (let  [e {:status (ref :stopped)
            :clear (ref false)
            :active-funcs (ref [])
            :pending-funcs (ref [])
            :shutdown-funcs (ref [])
            }]
    (dosync (alter engines conj e))
    e))


;; should do initialization of f on separate thread?
(defn engine-add-func [engine f]
  (dosync (alter (engine :pending-funcs) conj f)))

(defn engine-remove-func [engine f]
  (println "removing audio function")) 

(defn run-active-funcs [afs]
  (when afs
    (loop [[a & b] afs]
      (when a
        (a)
        (recur b))))) 


(defn engine-run [engine]
  (let [active-funcs (engine :active-funcs)
        pending-funcs (engine :pending-funcs)
        clear-flag (engine :clear)]
    (loop [counter 0]
      (when (= @(engine :status) :running)
      (run-active-funcs @active-funcs)
      (if @clear-flag
        (dosync
          (ref-set active-funcs [])
          (ref-set pending-funcs [])
          (ref-set clear-flag false))
        (do
          (when (seq @pending-funcs)
            (dosync
              (alter active-funcs concat @pending-funcs)
              (ref-set pending-funcs [])) 
            ) 
          (recur (inc counter))))))
    (println "stopping...")))

(defn engine-start [engine]
  "Starts an engine to run using a Thread.  User can optionally drive engine with another 
  timing source by setting the engine status to :running and calling engine-run"
  (when (= @(engine :status) :stopped)
    (dosync (ref-set (engine :status) :running))
    (.start (Thread. ^Runnable (partial engine-run engine)))))

(defn engine-stop [engine]
  "Stops an engine by altering the engine status, which signals the thread runner to stop."
  (when (= @(engine :status) :running)
    (dosync (ref-set (engine :status) :stopped))))

(defn engine-clear [engine]
  (if (= @(engine :status) :running)
    (dosync 
      (ref-set (engine :clear) true))
    (dosync 
      (ref-set (engine :active-funcs) [])
      (ref-set (engine :pending-funcs) []))))
    
                            
(defn engine-status [engine]
  @(:status engine))

(defn engine-kill-all []
  "Kills all engines and clears them"
  (dosync
    (loop [[a & b] @engines]
      (when a
        (engine-clear a)
        (recur b)
        ))))

(defn engines-clear []
  "Kills all engines and clears global engines list. Useful for development in REPL, but user must be 
  careful after clearing not to use existing engines."
  (engine-kill-all)
  (dosync (ref-set engines [])))

(comment 

(def t (engine-create))
(def c (atom 0))
  (engine-add-func t (fn [] 
                       (do
                         (println (swap! c inc))
                         (Thread/sleep 500))))
(engine-start t)
(engine-stop t)
(engine-clear t)
  

  )


