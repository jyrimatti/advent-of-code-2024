(ns advent-of-code-2024.day08
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as comb]))

(defn input
  [] (->
      "day08.txt"
      io/resource
      io/file
      slurp))

(defn bounds
  [grid]
  [(count grid) (count (first grid))])

(defn distance
  [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn inside?
  [[rows cols] [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row rows)
       (< col cols)))

(defn coordinate-pairs
  [[_ antenna]]
  (filter #(== 2 (count %)) (comb/subsets (map :coord antenna))))

(defn antenna-pairs
  [antennas]
  (mapcat coordinate-pairs (group-by :freq antennas)))

(defn deltas
  [pos dist]
  [(map + pos dist) (map - pos dist)])

(defn antinode1
  [bounds antenna-pair]
  (let [[c1 c2] antenna-pair
        dist (distance c1 c2)]
    (remove #{c1 c2} (filter #(inside? bounds %) (concat (deltas c1 dist) (deltas c2 dist))))))

(defn antinode2
  [bounds antenna-pair]
  (let [[c1 c2] antenna-pair
        dist (distance c1 c2)
        dists (iterate #(map + dist %) dist)]
     (concat (take-while #(inside? bounds %) (map #(map + c1 %) dists))
             (take-while #(inside? bounds %) (map #(map - c1 %) dists))
             (take-while #(inside? bounds %) (map #(map + c2 %) dists))
             (take-while #(inside? bounds %) (map #(map - c2 %) dists)))))

(defn antennas
  [grid [rows cols]]
  (for [row (range rows)
        col (range cols)
        :when (not= (get-in grid [row col]) \.)]
    {:freq (get-in grid [row col]) :coord [row col]}))

(defn result
  []
  (let [grid (vec (.split (input) "\n"))
        bounds (bounds grid)
        apairs (antenna-pairs (antennas grid bounds))]
  [(count (distinct (mapcat #(antinode1 bounds %) apairs)))
   (count (distinct (mapcat #(antinode2 bounds %) apairs)))]))
