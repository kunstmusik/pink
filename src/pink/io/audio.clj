(ns 
  ^{:doc "Functions for handling Audio I/O using Javasound"
   :author "Steven Yi"}
  pink.io.audio
  (:import [javax.sound.sampled AudioFormat AudioSystem SourceDataLine
                                TargetDataLine
                                AudioFileFormat$Type AudioInputStream]))


(defn open-line [audio-format buffer-size]
  (let [#^SourceDataLine line (AudioSystem/getSourceDataLine audio-format)]
    (doto line 
    (.open audio-format buffer-size)
    (.start))))

(defn open-input-line [audio-format]
  (let [#^TargetDataLine line (AudioSystem/getTargetDataLine audio-format)]
    (doto line 
    (.open audio-format)
    (.start))))


(defn print-java-sound-info
  "Print out available JavaSoundMixers"
  []
  (let [mixers (AudioSystem/getMixerInfo)
        cnt (alength mixers)]
    (println "Mixers Found: " cnt)
    (loop [indx 0]
      (when (< indx cnt)
        (let [mixer ^Mixer$Info (aget mixers indx)] 
          (println "Mixer " indx " :" mixer)
          (recur (unchecked-inc-int indx))
          )))))

