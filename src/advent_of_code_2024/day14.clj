(ns advent-of-code-2024.day14
  (:require
   [clojure.java.io :as io]))

(defn input
  []
  (-> "day14.txt"
      io/resource
      io/file
      slurp))

(defn parse-robot
  [line]
  (let [[[_ x y vx vy]] (re-seq #"p=([-0-9]+),([-0-9]+) v=([-0-9]+),([-0-9]+)" line)]
    [(parse-long x) (parse-long y) (parse-long vx) (parse-long vy)]))

(defn parse
  [input]
  (map parse-robot (.split input "\n")))

(defn move
  [[rows cols] [x y vx vy]]
  [(mod (+ x vx) cols) (mod (+ y vy) rows) vx vy])

(defn moves
  [bounds robots]
  (iterate #(mapv (partial move bounds) %) robots))

(defn quadrant
  [[rows cols] [x y _ _]]
  (let [dx (int (/ cols 2))
        dy (int (/ rows 2))]
    (cond
      (and (< x dx) (< y dy)) 1
      (and (< x dx) (> y dy)) 2 
      (and (> x dx) (< y dy)) 3
      (and (> x dx) (> y dy)) 4
      :else 0)))

(defn safety-factor
  [bounds robots q]
  (count (filter #(== (quadrant bounds %) q) robots)))

(defn is-xmas-tree
  [robots]
   (apply max (map count (vals (group-by first robots)))))

(defn result
  []
  (let [bounds [103 101]
        robots (parse (input))
        res (nth (moves bounds robots) 100)
        part1 (reduce * (map #(safety-factor bounds res %) [1 2 3 4]))
        part2 (drop-while #(< (is-xmas-tree (second %)) 31) (take-nth 103 (drop 156 (keep-indexed (fn [i x] [i x]) (moves bounds robots)))))]
    [part1 (first (first part2))]))


(defn print-robots
  [bounds robots]
  (doseq [j (range (first bounds))]
    (println (apply str (for [i (range (second bounds))]
                          (let [rs (count (filter #(and (== i (first %)) (== j (second %))) robots))]
                            (if (> rs 0)
                              (.toString rs)
                              " "))))))
  (println))