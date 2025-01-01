(ns advent-of-code-2024.day01
  (:require [clojure.java.io :as io]))

(defn input
  []
  (-> "day01.txt"
      io/resource
      io/file
      slurp))

(defn result
  []
  (let [pairs        (map #(.split % "\\s+") (.split (input) "\n"))
        as           (sort (map (comp parse-long first) pairs))
        bs           (sort (map (comp parse-long second) pairs))
        distances    (map (comp abs -) as bs)
        appearances  (map #(count (filter (partial = %) bs)) as)
        similarities (map * as appearances)]
    [(reduce + distances) (reduce + similarities)]))
