(ns advent-of-code-2024.day11
  (:require
   [clojure.java.io :as io]))

(defn input
  [] (->
      "day11.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (map (comp (fn [x] [x 1]) parse-long) (re-seq #"\d+" input)))

(defn num-digits [n x]
  (if (== 0 n)
    x
    (recur (quot n 10) (inc x))))

(defn change
  [[stone amount]]
  (if (== stone 0)
    [[1 amount]]
    (let [digits (num-digits stone 0)
          half (/ digits 2)]
      (if (even? digits)
        [[(parse-long (subs (str stone) 0 half)) amount]
         [(parse-long (subs (str stone) half)) amount]]
        [[(* stone 2024) amount]]))))

(defn sum-amounts
  [grouped]
  (update-vals grouped #(apply + (mapv second %))))

(defn blink
  [stone-and-amounts]
  (sum-amounts (group-by first (mapcat change stone-and-amounts))))

(defn result
  []
  (let [all (iterate blink (parse (input)))
        part1 (drop 25 all)
        part2 (drop 50 part1)]
    [(reduce + (map second (first part1)))
     (reduce + (map second (first part2)))]))
