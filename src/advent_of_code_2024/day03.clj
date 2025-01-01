(ns advent-of-code-2024.day03
  (:require [clojure.java.io :as io]))

(defn input
  []
  (-> "day03.txt"
      io/resource
      io/file
      slurp))

(defn products
  [input]
  (let [matches (re-seq #"mul\(([0-9]{1,3}),([0-9]{1,3})\)" input)]
    (map (comp (partial apply *) (partial map parse-long) rest) matches)))

(defn result
  []
  (let [blocks (.split (input) "do\\(\\)")
        dos (map #(first (.split % "don't\\(\\)")) blocks)
        allProducts (mapcat products dos)]
  [(reduce + (products (input))) (reduce + allProducts)]))
