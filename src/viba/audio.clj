(ns viba.audio
  (:require [clojure.core.async :as async :refer [<! >! chan close! go-loop mult tap]])
  (:import [java.io File IOException])
  (:import [javax.media.format AudioFormat])
  (:import [javax.sound.sampled AudioInputStream AudioSystem DataLine LineUnavailableException SourceDataLine]))

(defn read-chunks [^AudioInputStream audio-stream]
  (let [out (chan 16)]
    (go-loop [i 0]
      (let [^bytes buffer (byte-array 4096)
            bytes-read (int (.read audio-stream buffer 0 (alength buffer)))]
        (if (= -1 bytes-read)
          (close! out)
          (do (>! out {:buffer buffer :i i})
              (recur (inc i))))))
    out))

(defn read-file [filename]
  (try
    (let [^File sound-file (File. filename)
          ^AudioInputStream audio-stream (AudioSystem/getAudioInputStream sound-file)
          ^AudioFormat audio-format (.getFormat audio-stream)
          ^DataLine$Info info (javax.sound.sampled.DataLine$Info. SourceDataLine audio-format)
          ^SourceDataLine source-line (AudioSystem/getLine info)]
      (.open source-line audio-format)
      (.start source-line)
      (let [chunk-chan (mult (read-chunks audio-stream))
            chan-copy1 (tap chunk-chan (chan))
            chan-copy2 (tap chunk-chan (chan))]
        (go-loop []
          (let [x (<! chan-copy1)]
            (if-not x
              (do (.drain source-line)
                  (.close source-line))
              (recur))))
        chan-copy2))
    (catch Throwable t
      (println t))))

(defn play-chunk [^SourceDataLine line ^bytes buffer]
  (try
    (let [buffer-size (alength buffer)]
      (doto line
        (.write buffer 0 buffer-size)
        ;;(.drain)
        ))
    (catch Throwable t
      (println t))))
