(ns advent-of-code-2024.day18
  (:require
   [astar.core :as astar]
   [clojure.core.matrix :refer [pow]]
   [clojure.java.io :as io]
   [clojure.math :refer [sqrt]]
   [clojure.string :as string]))

(defn input
  []
  (-> "day18.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (mapv (comp reverse #(mapv parse-long %)) (map #(.split % ",") (.split input "\n"))))

(defn bounds
  [grid]
  [(count grid) (count (first grid))])

(def steps
  [[-1 0] [0 1] [1 0] [0 -1]])

(defn mk-grid
  [rows cols]
  (vec (repeat rows (vec (repeat cols \.)))))

(defn inside?
  [[rows cols] [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row rows)
       (< col cols)))

(defn graph
  [grid [row col]]
  (filter #(not= \# (get-in grid %))
          (filter (partial inside? (bounds grid))
                  (map #(map + [row col] %) steps))))

(defn heuristic
  [[row1 col1] [row2 col2]]
  (sqrt (+ (pow (- row1 row2) 2) (pow (- col1 col2) 2))))

(defn find-path
  [grid start end]
  (astar/route (partial graph grid) (fn [_ _] 1) (partial heuristic end) start end))

(defn corrupt-grid
  [grid bs amount]
  (reduce (fn [grid byte] (assoc-in grid byte \#)) grid (take amount bs)))

(defn result
  []
  (let [bs (parse (input))
        grid (mk-grid 71 71)
        start [0 0]
        end (map - (bounds grid) [1 1])
        part1 (find-path (corrupt-grid grid bs 1024) start end)
        part2 (drop-while (comp nil? second) (map (fn [amount] [amount
                                                                (find-path (corrupt-grid grid bs amount) start end)])
                                                  (reverse (range 1 (count bs)))))]
    [(count part1)
     (string/join "," (reverse (nth bs (first (first part2)))))]))
