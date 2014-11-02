(ns pink.io.sound-file
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffers]])
  (:import [java.io File ByteArrayOutputStream]
           [java.nio ByteBuffer]
           [javax.sound.sampled AudioFileFormat$Type AudioFormat
            AudioFormat$Encoding AudioInputStream AudioSystem]))

(defn- locate-file
  [f]
  (cond 
    (instance? File f) f
    (string? f) (File. ^String f)))

;(defn- get-target-format
;  [^AudioFormat src-format]
;  (AudioFormat. AudioFormat$Encoding/PCM_FLOAT
;                *sr* 
;                32 
;                (.getChannels src-format) 
;                (* 4 (.getChannels src-format)) ; frame size
;                *sr* 
;                true))

;(defn get-audio-data
;  "Converts to double[][]"
;  [^AudioInputStream astream]
;  )

;(defn sfload
;  "Load given file or filename as sampled audio.  Returns a map with meta-information as
;  well as audio split into discrete channels. Converts to doubles from source format."
;  [f]
;  (let [af ^File (locate-file f)
;        ain ^AudioInputStream (AudioSystem/getAudioInputStream af)
;        src-format ^AudioFormat (.getFormat ain)
;        target-format ^AudioFormat (get-target-format src-format)
;        astream ^AudioInputStream (AudioSystem/getAudioInputStream 
;                                    target-format ain) ]
;    { :channels (.getChannels target-format)
;     :format target-format 
;     :data (get-audio-data astream) 
;     })
;  )

(defn- convert-to-double-arrays
  "Convert 16-bit, big endian audio samples to +-1.0 double samples"
  [^ByteArrayOutputStream baos ^long channels]
  (let [barray ^bytes (.toByteArray baos)
        chan-length ^long (/ (alength barray) (* channels 2)) ; assumes 16-bit/2-byte per sample
        bbuffer (ByteBuffer/wrap barray)
        out (if (= channels 1) 
              (double-array chan-length)
              (into-array 
                (for [i (range channels)] (double-array chan-length))))]
    (loop [i 0]
      (if (< i chan-length)
        (do
          (if (= channels 1) 
            (aset ^doubles out i (double (/ (.getShort bbuffer) 32768.0)))
            (loop [j 0]
              (if (< j channels)
                (aset ^doubles (aget ^"[[D" out j) i (double (/ (.getShort bbuffer) 32768.0)))
                (recur (unchecked-inc j))) ))
          (recur (unchecked-inc i))) 
        out))))

(defn load-table
  "Load given file or filename as sampled audio.  Returns a map with meta-information as
  well as audio split into discrete channels. Converts to doubles from source format. 
  Currently only works with 16-bit PCM_SIGNED wave files."
  [f]
  (let [af ^File (locate-file f)
        ain0 ^AudioInputStream (AudioSystem/getAudioInputStream af)
        src-format ^AudioFormat (.getFormat ain0)
        ain ^AudioInputStream
        (AudioSystem/getAudioInputStream
          (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                        (long *sr*) 
                        16 
                        (.getChannels src-format) 
                        4 
                        (long *sr*) 
                        true)
          ain0) 
        channels (.getChannels src-format)
        baos (ByteArrayOutputStream.)
        buffer (byte-array 4096)]
    (loop [cnt (.read ain buffer)]
      (if (> cnt 0)
        (do 
          (.write baos buffer 0 cnt)
          (recur (.read ain buffer)))
        { :channels channels 
         ;:format target-format 
         :data (convert-to-double-arrays baos channels) 
         }))))

;(def a (load-table "/Users/stevenyi/work/csound/samples/akwf/AKWF_bw_sawbright/AKWF_bsaw_0001.wav"))

;(require '[clojure.pprint :refer [pprint]])
;(println (alength (:data a)))
;(pprint a)
