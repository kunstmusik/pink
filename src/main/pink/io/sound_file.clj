(ns pink.io.sound-file
  (:require [pink.config :refer :all]
            [pink.util :refer [create-buffers]])
  (:import [java.io File ByteArrayOutputStream FileOutputStream DataOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [javax.sound.sampled AudioFormat
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

;; TODO - finish implementation below and use for audio file writing...

(defn- write-wav-header!
  "Writes WAV file header.  Uses mock values for file and data chunk length
  that will be re-written once the WAV writing is complete and the file
  closed."
  [wav-data ^long sr ^long bit-rate ^long channels] 
  (let [baos (:byte-array wav-data)
        fos (:fos wav-data)
        dos (:dos wav-data)
        byte-rate (long (/ bit-rate 8))
        block-align (* channels byte-rate)
        bbuffer (ByteBuffer/allocate 44)
        ]

    (.order bbuffer ByteOrder/LITTLE_ENDIAN)


    ;; RIFF identifier 
    (.put bbuffer (.getBytes "RIFF"))

    ;;; file length - mock data for now 
    (.putInt bbuffer 0)
    ;view.setUint32(4, 32 + samples.length * 2, true);

    ;; RIFF type 
    (.put bbuffer (.getBytes "WAVE"))

    ;; format chunk identifier 
    (.put bbuffer (.getBytes "fmt "))

    ;; format chunk length 
    (.putInt bbuffer 16)

    ;;; sample format (raw) 
    (.putShort bbuffer 1)

    ;;; channel count 
    (.putShort bbuffer channels)

    ;;; sample rate 
    (.putInt bbuffer sr)

    ;;; byte rate (sample rate * block align) 
    (.putInt bbuffer (* sr channels))

    ;;; block align (channel count * bytes per sample) 
    (.putShort bbuffer (* byte-rate channels))

    ;;; bits per sample 
    (.putShort bbuffer bit-rate)
    ;;; data chunk identifier 
    (.put bbuffer (.getBytes "data"))
    ;;; data chunk length - mock data for now
    (.putInt bbuffer 0)

    (.write baos (.array bbuffer))

    (.writeTo baos fos)

    )

  )

(defn open-wave-write
  "Opens a WAV file for streaming writes. WAV will have a mock header written
  that will have full information written when close-wav-data is called."
  [filename sr bit-rate channels]

  (let [baos (ByteArrayOutputStream.)
        fos (FileOutputStream. (File. ^String filename))
        dos (DataOutputStream. baos)
        wav-data 
        {:byte-array baos
         :fos fos
         :dos dos
         :samples-written (atom 0)}
        ]  
    (write-wav-header! wav-data sr bit-rate channels)
    wav-data
    )
  )

(defn write-wav-data 
  "Appends new audio data to WAV file."
  [wav-data interleaved-audio] 
  
  )

(defn close-wav-data
  "Rewrites WAV file header with appropriate values for samples written."
  [wav-data]
  (.close (:byte-array wav-data)) 
  (.close (:fos wav-data)) 
  )


(comment

;; testing code while developing wave writing code

(let [wav (open-wave-write "testc.wav" 44100 16 2)]
  (close-wav-data wav) 
  )

)
