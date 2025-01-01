(ns advent-of-code-2024.day12
  (:require
   [clojure.java.io :as io]
   [clojure.set :refer [difference]]))

(defn input
  []
  (-> "day12.txt"
      io/resource
      io/file
      slurp))

(defn grid
  [input]
  (mapv vec (.split input "\n")))

(defn bounds
  [grid]
  [(count grid) (count (first grid))])

(defn inside?
  [[rows cols] [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row rows)
       (< col cols)))

(defn steps
  []
  [[-1 0] [0 1] [1 0] [0 -1]])

(defn next-steps
  [[row col]]
  (mapv #(mapv + [row col] %) (steps)))

(defn same-plot?
  [grid bounds plant step]
  (and (inside? bounds step) (= plant (get-in grid step))))

(defn find-plot
  [grid bounds [row col] plot]
  (let [plant (get-in grid [row col])]
    (reduce (fn [plot coord]
              (conj (find-plot grid bounds coord plot) coord))
            (conj plot [row col])
            (filter #(and (same-plot? grid bounds plant %) (nil? (plot %))) (next-steps [row col])))))

(defn all-coords
  [[rows cols]]
  (set (for [row (range rows)
             col (range cols)]
         [row col])))

(defn find-plots
  [grid bounds plots remaining]
  (let [next (first remaining)]
    (if next
      (let [plot (find-plot grid bounds next #{})]
        (recur grid bounds (cons plot plots) (difference remaining plot)))
      plots)))

(defn perims
  [plot [row col] n]
  (filter #(nil? (plot %)) [(nth (next-steps [row col]) n)]))

(defn plot-perimeters
  [plot n]
  (mapcat #(perims plot % n) plot))

(defn perimeter
  [plot n]
  (count (plot-perimeters plot n)))

(defn remove-elem
  [col elem]
  (concat (take-while #(not= elem %) col)
          (rest (drop-while #(not= elem %) col))))

(defn split-duplicates
  [sorted-vals]
  (let [deduped (vec (dedupe sorted-vals))]
    (if (empty? sorted-vals)
      []
      (cons deduped 
            (split-duplicates (vec (reduce (fn [acc elem]
                                             (if (empty? acc)
                                               acc
                                               (remove-elem acc elem)))
                                           sorted-vals
                                           deduped)))))))

(defn split-ranges
  [range]
  (mapcat #(reduce (fn [acc x]
                    (if (empty? acc)
                      [[x]]
                      (let [prev (last (peek acc))]
                        (if (= (inc prev) x)
                          (conj (pop acc) (conj (peek acc) x))
                          (conj acc [x])))))
          []
          %) (split-duplicates range)))

(defn sides
  [plot n]
  (vals (update-vals (group-by (if (even? n) first second) (plot-perimeters plot n))
                     (comp count split-ranges sort #(mapv (if (even? n) second first) %)))))

(defn score
  [plot]
  (* (count plot) (reduce + (map #(perimeter plot %) (range 0 4)))))

(defn price
  [plot]
  (* (count plot) (reduce + (mapcat #(sides plot %) (range 0 4)))))

(defn result
  []
  (let [grid (grid (input))
        bounds (bounds grid)
        plots (find-plots grid bounds [] (all-coords bounds))
        scores (map score plots)
        prices (map price plots)]
        [(reduce + scores) (reduce + prices)]))
