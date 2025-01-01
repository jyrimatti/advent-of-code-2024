(ns advent-of-code-2024.day13
  (:require
   [clojure.core.matrix :refer [inverse matrix mmul]]
   [clojure.java.io :as io]
   [clojure.math :as math]))

(defn input
  []
  (->
   "day13.txt"
   io/resource
   io/file
   slurp))

(defn parse-button
  [line]
  (let [[[_ x y]] (re-seq #"Button [A|B]: X[+](\d+), Y[+](\d+)" line)]
    {:x (parse-long x), :y (parse-long y)}))

(defn parse-prize
  [line]
  (let [[[_ x y]] (re-seq #"Prize: X[=](\d+), Y[=](\d+)" line)]
    {:x (parse-long x), :y (parse-long y)}))

(defn parse-machine
[lines]
  (let [[a b p] (.split lines "\n")]
    {:a (assoc-in (parse-button a) [:tokens] 3)
     :b (assoc-in (parse-button b) [:tokens] 1)
     :prize (parse-prize p)}))

(defn parse
  [input]
  (map parse-machine (.split input "\n\n")))

(defn cost
  [{:keys [a b]} [aa bb]]
  (if (nil? aa)
    nil
    (+ (* aa (:tokens a))
       (* bb (:tokens b)))))

(defn inc-price
  [n machine]
  (update-in (update-in machine [:prize :y] #(+ n %)) [:prize :x] #(+ n %)))

(defn is-integer?
  [n]
  (< (abs (- n (math/round n))) 0.01))

(defn solve
  [{:keys [a b prize]}]
  (let [m (matrix [[(:x a) (:x b)] [(:y a) (:y b)]])
        v (matrix [[(:x prize)] [(:y prize)]])
        res (mapv first (mmul (inverse m) v))]
    (if (every? is-integer? res)
      (map math/round res)
      nil)))

(defn calc-tokens
  [machines]
  (reduce + (filter #(not (nil? %)) (map #(cost % (solve %)) machines))))

(defn result
  []
  (let [machines (parse (input))]
    [(calc-tokens machines)
     (calc-tokens (map #(inc-price 10000000000000 %) machines))]))

