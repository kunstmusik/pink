(ns pink.audio.protocols
  "Protocols for Audio Engine")

(defprotocol AudioFunc
  (process [this] "process one *ksmps* block"))

(defn audio-func [a]
  "Utility function to convert a simple funciton into an AudioFunc"
  (reify AudioFunc 
    (process [this] (a) )))

(comment
  (defn test-audio-func [] 5)
  (def b (audio-func test-audio-func)))

