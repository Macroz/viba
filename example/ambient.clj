(ns viba.example.ambient
  (:require [brute.core :as brute])
  (:require [viba.core :as viba :refer :all])
  (:require [clojure.core.async :as async :refer [<! go-loop]]))

(async/go
  (let [player (test-pipe "/home/markku/dev/loxone/resources/music/ambient.wav")
        data (<! (async/into [] player))
        arr (mapcat :buffer data)]
    (try
      (spit "example/sound.svg"
            (brute/plot {:fill "#777"} (take 4410 arr)
                        {:fill "#393" :opacity 0.5} (average 2000 (take 4410 arr))))
      (catch Throwable t (println t)))))

#_(let [signal (repeatedly rand)]
  (spit "example/sound.svg"
        (brute/plot {:fill "#777"} (take 4410 signal)
                    {:fill "#393" :opacity 0.5} (average 2000 (take 4410 signal)))))
