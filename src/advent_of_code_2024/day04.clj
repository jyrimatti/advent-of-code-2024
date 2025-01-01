(ns advent-of-code-2024.day04
  (:require
   [clojure.java.io :as io]))

(defn input
  []
  (-> "day04.txt"
      io/resource
      io/file
      slurp))

(defn horizontal
  [rows]
  (let [candidates (map #(apply str %) (mapcat #(partition 4 1 %) rows))]
  (count (filter #(or (= "XMAS" %) (= "SAMX" %)) candidates))))

(defn vertical
  [rows]
  (horizontal (apply map vector rows)))

(defn candidates1
  [rows]
  (map-indexed (fn [i x] (apply str
                                (take (count (first rows))
                                      (drop i
                                            (concat x (repeat i " ")))))) rows))

(defn candidates2
  [rows]
  (map-indexed (fn [i x] (apply str
                                (concat (repeat (- (count (first rows)) i) " ")
                                        (take i x)))) rows))

(defn diagonal
  [rows]
    (+ (vertical (candidates1 rows))
       (vertical (candidates2 rows))
       (vertical (candidates1 (reverse rows)))
       (vertical (candidates2 (reverse rows)))))

(defn valids
  [rows]
  (for [row (range 1 (count rows))
        col (range 1 (count (first rows)))]
     (if (and (= (get-in rows [row col]) \A)
             (or
              (and (= (get-in rows [(- row 1) (- col 1)]) \M)
                   (= (get-in rows [(+ row 1) (+ col 1)]) \S)
                   (= (get-in rows [(- row 1) (+ col 1)]) \M)
                   (= (get-in rows [(+ row 1) (- col 1)]) \S))
              (and (= (get-in rows [(- row 1) (- col 1)]) \S)
                   (= (get-in rows [(+ row 1) (+ col 1)]) \M)
                   (= (get-in rows [(- row 1) (+ col 1)]) \M)
                   (= (get-in rows [(+ row 1) (- col 1)]) \S))
              (and (= (get-in rows [(- row 1) (- col 1)]) \M)
                   (= (get-in rows [(+ row 1) (+ col 1)]) \S)
                   (= (get-in rows [(- row 1) (+ col 1)]) \S)
                   (= (get-in rows [(+ row 1) (- col 1)]) \M))
              (and (= (get-in rows [(- row 1) (- col 1)]) \S)
                   (= (get-in rows [(+ row 1) (+ col 1)]) \M)
                   (= (get-in rows [(- row 1) (+ col 1)]) \S)
                   (= (get-in rows [(+ row 1) (- col 1)]) \M))))
    1 0)))


(defn result
  []
  (let [rows (.split (input) "\n")]
    [(reduce + [(horizontal rows) (vertical rows) (diagonal rows)])
     (reduce + (valids rows))]))

