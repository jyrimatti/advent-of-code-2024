(ns advent-of-code-2024.day10
  (:require
   [clojure.java.io :as io]))

(defn input
  [] (->
      "day10.txt"
      io/resource
      io/file
      slurp))

(defn grid
  []
  (mapv #(mapv (comp parse-long str) %)
        (map vec (.split (input) "\n"))))

(defn bounds
  [grid]
  [(count grid) (count (first grid))])

(defn inside?
  [[rows cols] [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row rows)
       (< col cols)))

(defn trailheads
  [grid]
  (let [[rows cols] (bounds grid)]
    (for [row (range rows)
          col (range cols)
          :when (== (get-in grid [row col]) 0)]
      [row col])))

(defn steps
  []
  [[-1 0] [1 0] [0 1] [0 -1]])

(defn valid-step?
  [grid bounds height]
  #(and (inside? bounds %)
        (== (get-in grid %) (inc height))))

(defn move
  [grid bounds position res]
  (let [height (get-in grid position)
        candidates (map #(map + position %) (steps))
        nextsteps (filterv (valid-step? grid bounds height) candidates)]
    (if (empty? nextsteps)
      (if (== height 9) [(cons position res)] nil)
      (mapcat #(move grid bounds % (cons position res)) nextsteps))))

(defn follow-trailhead
  [grid trail-processor]
  (comp count distinct trail-processor #(move grid (bounds grid) % [])))

(defn calculate
  [grid trail-processor]
  (reduce + (map (follow-trailhead grid trail-processor) (trailheads grid))))

(defn result
  []
  [(calculate (grid) #(map first %))
   (calculate (grid) identity)])