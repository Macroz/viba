(ns viba.core
  (:require [viba.audio :as audio :refer :all])
  (:require [viba.ops :as ops :refer :all])
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
