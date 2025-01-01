(ns advent-of-code-2024.day09
  (:require
   [clojure.java.io :as io]))

(defn input
  [] (->
      "day09.txt"
      io/resource
      io/file
      slurp))

(defn indexed []
  (map (fn [a b] [(parse-long (str a)) b]) (vec (input)) (range)))

(defn tag
  [[d index]]
  [d (if (even? index) (int (/ index 2)) \.)])

(defn tagged []
  (mapv tag (indexed)))

(defn blocks
  [tagged]
  (vec (mapcat #(apply repeat %) tagged)))

(defn defrag1
  [i j blocks result]
  (let [id (.get blocks i)]
    (if (> i j)
      result
      (if (= \. (.get blocks j))
        (recur i (dec j) blocks result)
        (if (= \. id)
          (recur (inc i) (dec j) blocks (cons (.get blocks j) result))
          (recur (inc i)      j  blocks (cons (.get blocks i) result)))))))

(defn has-space?
  [needed [available id]]
    (and (>= available needed) (= \. id)))

(defn defrag2
  [j result]
    (if (< j 0)
      result
      (let [[size id] (.get result j)]
        (if (= \. id)
          (recur (dec j) result)
          (let [[index [available _]] (first (keep-indexed #(when (has-space? size %2) [%1 %2]) (take j result)))]
            (if (some? index)
              (let [aa (subvec result 0 index)
                    bb (subvec result index j)
                    cc (subvec result j)]
                (recur j (vec (concat aa
                                      [[size id] [(- available size) \.]]
                                      (subvec bb 1)
                                      [[size \.]]
                                      (subvec cc 1)))))
              (recur (dec j) result)))))))

(defn compacted1 [blocks]
  (reverse (defrag1 0 (dec (count blocks)) blocks ())))

(defn compacted2 [tagged]
  (blocks (pmap (fn [[d b]] [d (if (= \. b) 0 b)]) (defrag2 (dec (count tagged)) tagged))))

(defn result
  []
  [(reduce + (map * (compacted1 (blocks (tagged))) (range)))
   (reduce + (map * (compacted2 (tagged)) (range)))])