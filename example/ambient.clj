(ns viba.example.ambient
  (:require [brute.core :as brute])
  (:require [viba.core :as viba])
  (:require [viba.audio :as audio])
  (:require [viba.ops :as ops])
  (:require [valo.hue :as hue])
  (:require [clojure.core.async :as async :refer [<! go-loop]])
  (:import [javax.sound.sampled AudioSystem DataLine AudioFormat SourceDataLine]))

(def transpose (partial apply map list))

(def abs #(Math/abs %))

(def last-avg (atom 0))

(defn analyze-chunk [chunk]
  (try
    (let [buffer (:buffer chunk)
          channels (transpose (partition 2 buffer))
          channels (map #(ops/average 50 %) channels)
          buffer (apply interleave channels)
          buffer (byte-array buffer)
          avg (/ (reduce + 0.0 (map abs (first channels))) (/ (alength buffer) 2))
          light (> avg @last-avg)]
      (reset! last-avg avg)
      (println "analyzed" (:i chunk) light)
      (assoc chunk
             ;;:buffer buffer
             :light light))
    (catch Throwable t
      (println t))))

(defn init-player []
  (let [buffer-size 44100 ; bytes
        ;;buffer (byte-array bufferSize)
        ^AudioFormat audio-format (AudioFormat. (float 44100.0) 16 2 true false)
        ^DataLine.Info info (javax.sound.sampled.DataLine$Info. SourceDataLine audio-format buffer-size)
        ^SourceDataLine line (AudioSystem/getLine info)]
    (doto line
      (.open audio-format buffer-size)
      (.start))
    {:line line}))

(defn close-player [player]
  (.close (:line player)))

(def light-last-state (atom false))

(defn play-chunk [player hue chunk]
  (try
    (when (not= @light-last-state (:light chunk))
      (reset! light-last-state (:light chunk))
      (if (:light chunk)
        (.set-light-hsl hue 6 0.0 100.0 50.0)
        (.set-light-hsl hue 6 0.0 0.0 50.0)))
    (audio/play-chunk (:line player) (:buffer chunk))
    (catch Throwable t
      (println t)))
  chunk)

(defn init-hue []
  (let [hue (hue/make-hue)]
    (doto hue
      (.set-server "http://192.168.0.101")
      (.set-user "markkurontu" nil))))

(defn test-pipe [filename]
  (let [player (init-player)
        hue (init-hue)
        read (audio/read-file filename)
        analyze (async/map analyze-chunk [read] 16)
        play (async/map (partial play-chunk player hue) [analyze] 1)]
    (go-loop []
      (if-let [x (<! play)]
        (do
          ;;(println "done-with" (:i x))
          (recur))
        (do
          (println "closing")
          (close-player player))))
    play))

(defn play []
  (async/go
    (let [play (test-pipe "/home/markku/dev/loxone/resources/music/ambient.wav")
          data (<! (async/into [] play))
          arr (mapcat :buffer data)]
      (try
        (spit "example/ambient.svg"
              (brute/plot {:fill "#777"} (take 4410 arr)
                          {:fill "#393" :opacity 0.5} (ops/average 2000 (take 4410 arr))))
        (catch Throwable t (println t))))))

(defn plot-random []
  (let [signal (repeatedly rand)]
    (spit "example/random.svg"
          (brute/plot {:fill "#777"} (take 4410 signal)
                      {:fill "#393" :opacity 0.5} (ops/average 2000 (take 4410 signal))))))
