(ns advent-of-code-2024.day06
  (:require
   [clojure.java.io :as io]))

(defn input
  []
  (-> "day06.txt"
      io/resource
      io/file
      slurp))

(defn bounds
  [grid]
  [(count grid) (count (first grid))])

(defn obstruction?
  [grid pos]
  (= (get-in grid pos) "#"))

(defn find-positions
  [grid [rows cols] pred]
  (for [row (range rows)
        col (range cols)
        :when (pred (get-in grid [row col]))]
    [row col]))

(defn movement
  [guard]
  (case guard
    "^" [-1 0]
    "v" [1 0]
    ">" [0 1]
    "<" [0 -1]))

(defn turn
  [dir]
  (case dir
    "^" ">"
    "v" "<"
    ">" "v"
    "<" "^"))

(defn inside?
  [[rows cols] [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row rows)
       (< col cols)))

(defn move
  [grid bounds guardPos]
  (let [tags (get-in grid guardPos)
        guard (str (last tags))
        nextPos (mapv + guardPos (movement guard))]
    (if-not (inside? bounds nextPos)
      grid
      (if (obstruction? grid nextPos)
        (recur (assoc-in grid guardPos (str tags (turn guard))) bounds guardPos)
        (let [nextTags (get-in grid nextPos)]
          (when (== -1 (.indexOf nextTags guard))
            (recur (assoc-in grid nextPos (str nextTags guard)) bounds nextPos)))))))

(defn visited?
  [x]
  (or (> (count x) 1) (= "^" x)))

(defn result
  []
  (let [grid (mapv (comp vec #(map str %) vec) (.split (input) "\n"))
        bounds (bounds grid)
        guardPosition (first (find-positions grid bounds #(= "^" %)))
        guardMovements (move grid bounds guardPosition)
        obstaclePositions (filter #(not= guardPosition %) (find-positions guardMovements bounds visited?))
        withObstacles (map #(assoc-in grid % "#") obstaclePositions)]
    [(reduce + (map #(count (filter visited? %)) guardMovements))
     (count (filter #(nil? %) (pmap #(move % bounds guardPosition) withObstacles)))]))
