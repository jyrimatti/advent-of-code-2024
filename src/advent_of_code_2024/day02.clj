(ns advent-of-code-2024.day02
  (:require [clojure.java.io :as io]))

(defn input
  []
  (-> "day02.txt"
      io/resource
      io/file
      slurp))

(defn valid-sign?
  [report]
  (or (apply > report)
      (apply < report)))

(defn valid-diff?
  [report]
  (every? #(and (>= % 1) (<= % 3))
          (map (comp abs #(apply - %)) (partition 2 1 report))))

(defn valid?
  [report]
  (and (valid-sign? report) (valid-diff? report)))

(defn dampen
  [report]
  (mapv (fn [i] (keep-indexed #(when-not (== %1 i) %2) report))
        (range (count report))))

(defn result
  []
  (let [reports (map #(map parse-long %) (map #(.split % " ") (.split (input) "\n")))
        safe (filter valid? reports)
        safeDampened (filter (comp not-empty (partial filter valid?) dampen) reports)]
  [(count safe) (count safeDampened)]))
