(ns advent-of-code-2024.day23
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as comb]
   [clojure.string :refer [join]]))

(defn input
  []
  (-> "day23.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (mapv #(vec (.split % "-")) (.split input "\n")))

(defn contained-in-others
  [m cs x]
  (every? #(contains? (m x) %) (filter #(not= x %) cs))
  )

(defn groups-of
  [n m]
  (distinct (for [k (keys m)
                  cs (comb/combinations (m k) (dec n))
                  :when (every? #(contained-in-others m cs %) cs)
                  ]
              (set (conj cs k)))))

(defn result
  []
  (let [connections (parse (input))
        m (update-vals (merge-with into
                                   (update-vals (group-by first connections) #(mapv second %))
                                   (update-vals (group-by second connections) #(mapv first %))) set)
        max-network-size (inc (apply max (map count (vals m))))
        all-groups (mapcat #(groups-of % m) (reverse (range 3 (inc max-network-size))))
        ]
  [(count (filter (partial some #(.startsWith % "t")) (groups-of 3 m))) 
   (join "," (sort (first all-groups)))]))
