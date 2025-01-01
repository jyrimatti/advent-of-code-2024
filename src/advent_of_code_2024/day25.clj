(ns advent-of-code-2024.day25
  (:require
   [clojure.java.io :as io]))

(defn input
  []
  (-> "day25.txt"
      io/resource
      io/file
      slurp))

(defn transpose
  [m]
  (apply mapv vector m))

(defn parse-lock
  [lock]
  (mapv (comp count (partial take-while #(= \# %)))
        (transpose (mapv vec (next (.split lock "\n"))))))

(defn parse-key
  [key]
  (mapv (comp dec count (partial drop-while #(= \. %)))
        (transpose (mapv vec (next (.split key "\n"))))))

(defn parse-part
  [part]
  (if (.startsWith part "#")
    [[] [(parse-lock part)]]
    [[(parse-key part)] []]))

(defn parse
  [input]
  (let [parts (mapv parse-part (.split input "\n\n"))]
    [(mapcat first parts) (mapcat second parts)]))

(defn fits
  [[lock key]]
  (every? #(<= % 5) (map + lock key)))

(defn combinations
  [locks keys]
  (for [lock locks
        key keys]
    [lock key]))

(defn result
  []
  (let [[locks keys] (parse (input))]
    [(count (filter fits (combinations locks keys))) ]))
