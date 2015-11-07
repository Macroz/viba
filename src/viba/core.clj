(ns viba.core
  (:require [brute.core :as brute])
  (:require [viba.audio :as audio])
  (:require [clojure.core.async :as async :refer [<! go-loop]]))

(defn analyze-chunk [x]
  (println "analyzing" (:i x))
  x)

(defn play-chunk [x]
  (println "playing" (:i x))
  x)

(defn test-pipe [filename]
  (let [reader (audio/read-file filename)
        analyzer (async/map analyze-chunk [reader] 16)
        player (async/map play-chunk [analyzer])]
    (go-loop []
      (when-let [x (<! player)]
        (println "done-with" (:i x))
        (recur)))
    player))

#_(test-pipe "/home/markku/dev/loxone/resources/music/ambient.wav")


(defn low-pass
  ([^floats arr]
   (let [result (float-array (alength arr))
         l (int (dec (alength arr)))
         r (int (dec l))]
     (aset result 0 (aget arr 0))
     (aset result 1 (aget arr 1))
     (aset result l (aget arr l))
     (aset result r (aget arr r))
     (loop [i 2]
       (if (= i r)
         (floats result)
         (do
           (aset result i (* (+ (aget arr (dec (dec i)))
                                (aget arr (dec i))
                                (aget arr i)
                                (aget arr (inc i))
                                (aget arr (inc (inc i)))
                                )
                             0.2))
           (recur (inc i)))))))
  ([^floats arr n]
   (loop [i (int n)
          arr (floats arr)]
     (if (= i 0)
       arr
       (recur (dec i) (low-pass arr))))))

(defn average [n coll]
  (seq (low-pass (float-array coll) n)))

(def signal
  (map (fn [x]
         (let [x (+ 0.01 x)]
           (* (/ 19.0 (Math/pow x 0.331))
              (+ (* 1.21 (Math/sin (* 381.1 x)))
                 (* 2.71 (Math/sin (* 0.5 x)))
                 (* 1.70 (Math/sin (* 2.7 x)))
                 (* 0.92 (Math/sin (* 11.3 x)))
                 (* 0.50 (Math/sin (* 32.3 x)))
                 (* 0.13 (Math/sin (* 123.7 x)))
                 ))))
       (map #(+ 2.3 (* 0.01 %)) (range))))

(spit "example/signal.svg"
      (brute/plot {} (take 1280 signal)
                  {:fill "#393" :opacity 0.5} (average 1024 (take 1280 signal))))

(defn analyze-amplitude [m ^double x]
  (let [^double min (:min m 0.0)
        ^double max (:max m 0.0)]
    {:min (if (< x min) x min)
     :max (if (> x max) x max)}))

(defn resample [n coll]
  (map (fn [bucket]
         (let [m (reduce analyze-amplitude {} bucket)
               avg (/ (apply + bucket) (count bucket))]
           (-> m
               (assoc :avg avg)
               (assoc :rnd (rand-nth bucket))
               (assoc :first (first bucket)))))
       (partition-all (/ (count coll) n) coll)))

(async/go
  (let [player (test-pipe "/home/markku/dev/loxone/resources/music/ambient.wav")
        data (<! (async/into [] player))
        arr (mapcat :buffer data)]
    (try 
      (spit "sound.svg" (brute/plot {} (take 4410 signal) #_(take 4410 (drop 2205 arr))))
      (catch Throwable t (println t)))))


(spit "example/20.svg" (brute/plot {:fill "black"} (repeatedly 200 #(- (rand) 0.5))
                             {:fill "green" :opacity "0.5"} (repeatedly 20 #(- (rand) 0.5))))
