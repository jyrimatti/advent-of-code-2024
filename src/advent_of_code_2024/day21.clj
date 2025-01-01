(ns advent-of-code-2024.day21
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as comb]))

(defn input
  []
  (-> "day21.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (mapv vec (.split input "\n")))

(def keypad1
  [[\7 \8 \9]
   [\4 \5 \6]
   [\1 \2 \3]
   [\_\0 \A]])

(def keypad2
  [[\_ \^ \A]
   [\< \v \>]])

(defn find-node
  [grid node]
  (first (for [row (range (count grid))
               col (range (count (first grid)))
               :when (= (get-in grid [row col]) node)]
           [row col])))

(defn diff
  [keypad from to]
  (let [[a b] (find-node keypad from)
        [c d] (find-node keypad to)]
    [(- c a) (- d b)]))

(defn to-sort-order
  [x]
  (.indexOf [\< \^ \v \> \A] x))

(defn legal
  [a [b c d]]
  (not (or
    (= [a b c d] [\7 \v \v \v])
    (= [a b c]   [\4 \v \v])
    (= [a b]     [\1 \v])
    (= [a b]     [\0 \<])
    (= [a b c]   [\A \< \<])
    (= [a b]     [\^ \<])
    (= [a b]     [\< \^]))))

(defn moves
  [keypad from to]
  (let [[dr dc] (diff keypad from to)
        moves (concat
               (if (< dr 0) (repeat (abs dr) \^) [])
               (if (< dc 0) (repeat (abs dc) \<) [])
               (if (> dc 0) (repeat dc \>) [])
               (if (> dr 0) (repeat dr \v) []))]
    (conj (first (sort-by #(mapv to-sort-order (dedupe %))
                          (filter #(legal from %)
                                  (map vec (comb/permutations moves))))) \A)))

(defn all-moves
  [f keypad code]
  (second (reduce (fn [[prev ms] c]
                    [c (conj ms (f keypad prev c))])
                  [\A []]
                  code)))

(defn count-moves
  [self n code]
  (if (== n 0)
    (count code)
    (reduce + (map #(self self (dec n) %) (all-moves moves keypad2 code)))))

(defn solve
  [levels code]
  (let [cached-count-moves (memoize count-moves)]
  (count-moves cached-count-moves levels (apply concat (all-moves moves keypad1 code)))))

(defn complexity
  [code s]
  (* s (parse-long (apply str (filter #(Character/isDigit %) code)))))

(defn result
  []
  (let [codes (parse (input))]
    [(reduce + (map complexity codes (map #(solve 2 %) codes)))
     (reduce + (map complexity codes (map #(solve 25 %) codes)))]))
