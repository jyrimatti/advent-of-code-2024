(ns advent-of-code-2024.day20
  (:require
   [clojure.java.io :as io]
   [clojure.data.priority-map :refer [priority-map]]))

(defn input
  [] (->
      "day20.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (let [lines (.split input "\n")]
    (mapv vec lines)))

(defn find-node
  [grid node]
  (first (for [row (range (count grid))
               col (range (count (first grid)))
               :when (= (get-in grid [row col]) node)]
           [row col])))

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

(defn neighbors
  [grid end [row col timer]]
  (filter (fn [[a b]] (not= \x (get-in grid [a b])))
          (filter (fn [[a b]] (if (or (nil? timer) (<= timer 0)) (not= \# (get-in grid [a b])) true))
                  (filter #(inside? (bounds grid) %)
                          (map #(if (or (nil? timer) (= % end)) % (conj % (dec timer)))
                               (map #(mapv + [row col] %) steps))))))

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
                      (let [new-cost (inc c)]
                        (if (or (not (contains? costs neighbor))
                                (<= new-cost (costs neighbor)))
                          [(assoc qs neighbor new-cost)
                           (update cs neighbor (fnil min new-cost) new-cost)
                           (update ps neighbor (fnil conj '()) (conj (paths current) current))]
                          [qs cs ps])))
                    [queue costs paths]
                    (neighbors grid end current))]
        (if (= (take 2 current) (take 2 end))
          [new-costs, new-paths]
          (recur (pop new-queue) new-costs new-paths))))))

(defn to-paths
  [graph path]
  (loop [pr [[path graph]]]
    (if (every? (comp #(= % ()) second) pr)
      (map first pr)
      (recur (mapcat (fn [pg]
                       (if (= () (second pg))
                         [pg]
                         (map (fn [gg] [(cons (first gg) (first pg)) (rest gg)]) (second pg))))
                     pr)))))

(defn distance
  [[a b] [c d]]
  (+ (abs (- a c)) (abs (- b d))))

(defn cheats
  [costs fastest path available-cheats]
  (for [f (range (count path))
        t (range (inc f) (count path))
        ;;max-cheats (range 1 21)
        max-cheats (range 1 (inc available-cheats))
        :let [from (path f)
              to (path t)
              so-far (costs from)
              cheats-consumed (distance from to)
              to-end (- fastest (costs to))]
        :when (<= cheats-consumed max-cheats)]
    [(- fastest (+ so-far (distance from to) to-end)) from to]))

(defn calc-cheats
  [all-cheats]
  (update-vals
   (group-by identity
             (vals
              (update-vals
               (group-by #(drop 1 %) all-cheats)
               (comp #(apply max %) #(map first %)))))
   count))

(defn result
  []
  (let [grid (parse (input))
        start (find-node grid \S)
        end (find-node grid \E)
        [costs paths] (find-routes grid start end)
        fastest (costs end)
        main-route (vec (first (to-paths (paths end) (cons end nil))))
        cheat-counts1 (calc-cheats (cheats costs fastest main-route 2))
        cheat-counts2 (calc-cheats (cheats costs fastest main-route 20))]
    [(reduce + (map second (filter #(>= (first %) 100) cheat-counts1)))
     (reduce + (map second (filter #(>= (first %) 100) cheat-counts2)))]))