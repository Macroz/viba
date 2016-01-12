(ns viba.example.ambient
  (:require [brute.core :as brute])
  (:require [viba.core :as viba])
  (:require [viba.audio :as audio])
  (:require [viba.ops :as ops])
  (:require [valo.hue :as hue])
  (:require [valo.loxone :as loxone])
  (:require [clojure.core.async :as async :refer [<! go-loop]])
  (:import [javax.sound.sampled AudioSystem DataLine AudioFormat SourceDataLine]))

(defn mag (^double [^double a ^double b]
                   (Math/sqrt (+ (* a a) (* b b)))))

(defn energy-in [^bytes arr]
  (loop [energy (float 0.0)
         i (dec (/ (alength arr) 2))]
    (if (< i 0) (/ energy (alength arr))
        (recur (+ energy (mag (aget arr (* i 2)) (aget arr (inc (* i 2))))) (dec i)))))

(def transpose (partial apply map list))

(def abs #(Math/abs %))

(def last-avg (atom 0))
(def avg-energy (atom 0))

(defn analyze-chunk [chunk]
  (try
    (let [buffer (:buffer chunk)
          instant-energy (energy-in buffer)
          channels (transpose (partition 2 buffer))
          channels (map #(ops/average 50 %) channels)
          buffer (apply interleave channels)
          buffer (byte-array buffer)
          light (> instant-energy (* 1.1 @avg-energy))]
      (swap! avg-energy #(+ (* 0.9 %) (* 0.1 instant-energy)))
      (println "analyzed" (:i chunk) instant-energy @avg-energy light)
      (assoc chunk
             ;;:buffer buffer
             :light light))
    (catch Throwable t
      (println t))))

(defn init-player []
  (println "init-player")
  (let [buffer-size 8128 ; bytes
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
(def light-last-n (atom 0))

(defn play-chunk [player lights chunk]
  (try
    (println "playing" (:i chunk))
    (when (not= @light-last-state (:light chunk))
      (reset! light-last-state (:light chunk))
      #_(if (:light chunk)
        (.set-light-hsl lights (+ (mod @light-last-n 3) 1) 230.0 100.0 50.0)
        (.set-light-hsl lights (+ (mod @light-last-n 3) 1) 230.0 0.0 0))
      (if (:light chunk)
          (.set-light-hsl lights (str "AI" (+ 4 (mod @light-last-n 7))) 200.0 50.0 80.0)
          (.set-light-hsl lights (str "AI" (+ 4 (mod @light-last-n 7))) 200.0 0.0 0.0))
      (swap! light-last-n inc))
    (audio/play-chunk (:line player) (:buffer chunk))
    (catch Throwable t
      (println t)))
  (println "played" (:i chunk))
  chunk)

(defn init-hue []
  (let [hue (hue/make-hue)]
    (doto hue
      (.set-server "http://192.168.0.101")
      (.set-user "markkurontu" nil))))

(defn init-loxone []
  (let [loxone (loxone/make-loxone)]
    (doto loxone
      (.set-server "http://loxone.nitor.fi")
      (.set-user "admin" "admin")
      (.set-controller "0870d46b-033f-0c84-ffffeee00050010b")
      (.calibrate 1.0 0.79 0.68))
    (doseq [i (range 1 13)]
      (.set-light-hsl loxone (str "AI" i) 0 0 0))
    loxone))

(defn test-pipe [filename]
  (let [player (init-player)
        loxone (init-loxone)
        ;;hue (init-hue)
        read (audio/read-file filename)
        analyze (async/map analyze-chunk [read] 64)
        play (async/map (partial play-chunk player loxone #_hue) [analyze] 1)]
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
          arr (mapcat :buffer data)
          lights (map (fn [x] (if (:light data) 100 1)) data)]
      (try
        (spit "example/ambient.svg"
              (brute/plot {:fill "#777"} (take 4096 arr)
                          ;;{:fill "#393" :opacity 0.5} (ops/average 2000 (take 44100 arr))
                          {:fill "#cc0"} (take 1 lights)))
        (catch Throwable t (println t))))))

(defn plot-random []
  (let [signal (repeatedly rand)]
    (spit "example/random.svg"
          (brute/plot {:fill "#777"} (take 4410 signal)
                      {:fill "#393" :opacity 0.5} (ops/average 2000 (take 4410 signal))))))
