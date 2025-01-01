(ns advent-of-code-2024.day15
  (:require
   [clojure.java.io :as io]))

(defn input
  []
  (-> "day15.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [[grid movements]]
  [(mapv vec (.split grid "\n")) (.replace movements "\n" "")])

(defn movement
  [dir]
  (case dir
    \^ [-1 0]
    \v [1 0]
    \> [0 1]
    \< [0 -1]))

(defn bounds
  [grid]
  [(count grid) (count (first grid))])

(defn move
  [grid subject-loc step]
  (let [subject (get-in grid subject-loc)
        target-loc (mapv + subject-loc (movement step))
        target (get-in grid target-loc)]
    (if (or (= subject \.) (= target \#))
      nil
      (if (= target \.)
        [(assoc-in (assoc-in grid target-loc subject)
                   subject-loc \.) target-loc]
        (let [[next-grid_ next-location_] (move grid target-loc step)
              [next-grid next-location] (cond
                              (nil? next-location_) nil
                              (and (= target \[) (or (= step \^) (= step \v))) (move next-grid_ (mapv + target-loc [0 1]) step)
                              (and (= target \]) (or (= step \^) (= step \v))) (move next-grid_ (mapv + target-loc [0 -1]) step)
                              :else [next-grid_ next-location_])]
          (if (nil? next-location)
            nil
            [(assoc-in (assoc-in next-grid target-loc subject)
                       subject-loc \.)
             target-loc]))))))

(defn find-subject
  [grid [rows cols] subject]
  (for [row (range rows)
        col (range cols)
        :when (= (get-in grid [row col]) subject)]
    [row col]))

(defn coordinate
  [[row col]]
  (+ (* row 100) col))

(defn widen
  [x]
  (case x
    \# [\# \#]
    \. [\.\.]
    \@ [\@ \.]
    \O [\[ \]]))

(defn moves
  [grid robot-loc movements]
  (first (reduce (fn [[grid subject-loc] next-move]
            (let [result (move grid subject-loc next-move)]
              (if (nil? result)
                [grid subject-loc]
                result)))
          [grid robot-loc]
          movements)))

(defn solve
  [grid movements]
  (let [bounds (bounds grid)
        robot-loc (first (find-subject grid bounds \@))
        result (moves grid robot-loc movements)
        subjects (concat (find-subject result bounds \O)
                         (find-subject result bounds \[))]
  (reduce + (map coordinate subjects))))

(defn result
  []
  (let [[grid movements] (parse (.split (input) "\n\n"))
        part1 (solve grid movements)
        part2 (solve (mapv #(vec (mapcat widen %)) grid) movements)]
  [part1 part2]))
