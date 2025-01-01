(ns advent-of-code-2024.day16
  (:require
   [clojure.java.io :as io]
   [clojure.data.priority-map :refer [priority-map]]))

(defn input
  []
  (-> "day16.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (let [lines (.split input "\n")]
    lines))

(defn direction
  [[row1 col1] [row2 col2]]
  (cond
    (> row2 row1) \v
    (< row2 row1) \^
    (> col2 col1) \>
    (< col2 col1) \<))

(defn angle
  [dir1 dir2]
  (cond
    (= dir1 dir2) 0
    (or (and (= dir1 \<) (= dir2 \>))
        (and (= dir1 \>) (= dir2 \<))
        (and (= dir1 \^) (= dir2 \v))
        (and (= dir1 \v) (= dir2 \^))) 180
    :else 90))

(defn dist
  [[a1 b1 dir1] [a2 b2 dir2]]
  (case (angle (if (nil? dir1) (direction [a1 b1] [a2 b2]) dir1)
               (if (nil? dir2) (direction [a1 b1] [a2 b2]) dir2))
    0      1
    90  1000
    180 2000))

(def steps
  [[-1 0] [0 1] [1 0] [0 -1]])

(defn bounds
  [grid]
  [(count grid) (count (first grid))])

(defn inside?
  [[rows cols] [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row rows)
       (< col cols)))

(defn move-from
  [a b dir]
  (case dir
    \^ [[(inc a) b dir]]
    \> [[a (dec b) dir]]
    \v [[(dec a) b dir]]
    \< [[a (inc b) dir]]
    nil []))

(defn not-wall?
  [grid]
  #(not= \# (get-in grid (take 2 %))))

(defn neighbors
  [grid end [row col dir]]
  (if (= [row col] end)
    (map (fn [[a b]] [a b (direction [a b] [row col])])
         (filter #(and (not-wall? grid) (inside? (bounds grid) %))
                 (map #(map + [row col] %) steps)))
    (filter (not-wall? grid)
            (filter #(inside? (bounds grid) %)
                    (concat (move-from row col dir)
                            (map (fn [d] [row col d])
                                 (filter #(not= dir %) [\^ \> \v \<])))))))

(defn find-routes
  [grid start end]
  (loop [queue (priority-map start 0)
         costs {start 0}
         paths {start []}]
    (if (empty? queue)
      {}
      (let [[current c] (peek queue)
            [new-queue new-costs new-paths]
              (reduce (fn [[qs cs ps] neighbor]
                        (let [new-cost (+ c (dist current neighbor))]
                          (if (or (not (contains? costs neighbor))
                                  (<= new-cost (costs neighbor)))
                            [(assoc qs neighbor new-cost)
                             (update cs neighbor (fnil min new-cost) new-cost)
                             (update ps neighbor (fnil concat []) (conj (paths current) current))]
                            [qs cs ps])))
                      [queue costs paths]
                      (neighbors grid end current))]
        (if (= (take 2 current) (take 2 end))
          [new-costs, new-paths]
          (recur (pop new-queue) new-costs new-paths))))))

(defn find-node
  [grid [rows cols] node]
  (first (for [row (range rows)
               col (range cols)
               :when (= (get-in grid [row col]) node)]
           [row col])))

(defn result
  []
  (let [grid (parse (input))
        bounds (bounds grid)
        start (conj (find-node grid bounds \S) \>)
        end (find-node grid bounds \E)
        [all-costs all-paths] (find-routes grid start end)]
    [(second (first (filter #(= (take 2 (first %)) end) all-costs)))
     (count (distinct (map #(take 2 %) (cons end (mapcat second (filter #(= (take 2 (first %)) end) all-paths))))))]))