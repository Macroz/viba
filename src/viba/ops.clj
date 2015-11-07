(ns viba.ops)

(defn- low-pass
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

(defn resample [n coll]
  (map (fn [bucket]
         (let [avg (/ (apply + bucket) (count bucket))]
           avg))
       (partition-all (/ (count coll) n) coll)))
