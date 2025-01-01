(ns advent-of-code-2024.day07
  (:require
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.math.combinatorics :as comb]))

(defn input
  [] 
  (-> "day07.txt"
      io/resource
      io/file
      slurp))

(defn split-nums
  [[value nums]]
  [(parse-long value) (mapv parse-long (mapcat rest (re-seq #"\s*([^ ]+)\s*" nums)))])

(defn parse
  [input]
  (let [lines (map (comp rest first #(re-seq #"(.*):(.*)" %)) (.split input "\n"))]
    (map split-nums lines)))

(defn calc
  [p1 p2]
  (if (nil? (second p2))
    ;; end
    (if (== (count p1) 2)
      ((second p1) (first p1) (first p2))
      ((first p1) (first p2)))
    (if (== (count p1) 2)
      [(partial (second p2) ((second p1) (first p1) (first p2)))]
      [(partial (second p2) ((first p1) (first p2)))])))

(defn calculate
  [nums selection]
  (reduce calc (map vector nums (conj (vec selection) nil))))

(defn permutations
  [ops nums]
  (comb/selections ops (dec (count nums))))

(defn evaluate
  [ops [value nums]]
  (some #(== value %) (map #(calculate nums %) (permutations ops nums))))

(defn solve
  [ops data]
  ((juxt filter remove) #(evaluate ops %) data))

(defn num-digits [n x]
  (if (== 0 n)
    x
    (recur (quot n 10) (inc x))))

(defn concat-digits
  [num1 num2]
  (+ (* num1 (int (math/pow 10 (num-digits num2 0)))) num2))

(defn result
  []
  (let [data (parse (input))
        [succeeded failed] (solve [+ *] data)
        part1 (reduce + (map first succeeded))
        [succeeded2 _] (solve [+ * concat-digits] failed)]
    [part1
     (reduce + (cons part1 (map first succeeded2)))]))
