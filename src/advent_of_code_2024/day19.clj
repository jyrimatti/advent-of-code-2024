(ns advent-of-code-2024.day19
  (:require
   [clojure.java.io :as io]
   [trie.core :refer [trie]]))

(defn input
  [] 
  (-> "day19.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (let [lines (.split input "\n")
        patterns (.split (first lines) ", ")
        designs (rest (rest lines))]
   [patterns designs]))

(defn match
  [patterns design orig]
  (if (empty? design)
    [[orig]]
    (let [suitable (filter #(.startsWith design %) patterns)]
      (first (filter (comp not empty?) (map #(match patterns (subs design (count %)) orig) suitable))))))

(defn join
  [original additions-map]
  (let [ret (group-by first (mapcat (fn [[design cnt]]
                                      (let [found (additions-map design)]
                                        (if (nil? found)
                                          [(if (empty? design)
                                             [design 1]
                                             [nil 0])]
                                          (map (fn [pat] [(subs design (count pat)) cnt]) found))))
                                         original))
        rr (update-vals ret #(reduce + (map second %)))]
    [(dissoc rr "" nil) (or (rr "") 0)]))

(defn number-of-ways
  [patterns designs finished]
  (let [design-trie (trie (keys designs))
        fitting-prefixes (mapcat #(map (fn [design] [design %]) (design-trie %)) patterns)
        additions-map (update-vals (group-by first fitting-prefixes) #(mapv second %))
        [new-desingd fin] (join designs additions-map)]
    (if (empty? additions-map)
      (+ finished fin)
      (recur patterns new-desingd (+ finished fin)))))

(defn result
  []
  (let [[patterns designs] (parse (input))
        can-be-made (filterv not-empty (map first (mapcat #(match patterns % %) designs)))]
  [(count can-be-made) 
   (number-of-ways patterns (zipmap can-be-made (repeat 1)) 0)
   ]))
