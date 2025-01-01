(ns advent-of-code-2024.day22
  (:require
   [clojure.java.io :as io]))

(defn input
  []
  (-> "day22.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (mapv parse-long (.split input "\n")))

(defn mix-n-prune
  [x sn]
  (mod (bit-xor x sn) 16777216))

(defn evolve
  [sn]
  (let [sn1 (mix-n-prune (* sn 64) sn)
        sn2 (mix-n-prune (quot sn1 32) sn1)
        sn3 (mix-n-prune (* sn2 2048) sn2)]
    sn3))

(defn prices-and-changes
  [sn]
  (let [sns (iterate evolve sn)
        prices (map #(mod % 10) sns)
        changes (cons nil (map - (next prices) prices))]
  (next (map vector prices changes))))

(defn price-and-sequence
  [window]
  (let [changes (mapv second window)
        selling-price (first (last window))]
    [selling-price changes]))

(defn all-sequences
  [prs-n-cns]
  (map price-and-sequence (partition 4 1 prs-n-cns)))

(defn price-map
  [prs-n-cns]
  (update-vals (group-by second (all-sequences prs-n-cns))
               (comp first #(map first %))))

(defn best-sequence
  [price-maps]
  (let [all-sequences (distinct (mapcat #(map first %) price-maps))]
    (last (sort-by second (map (fn [sequ]
                                 [sequ
                                  (reduce + (map #(or (% sequ) 0) price-maps))
                                  (map #(or (% sequ) 0) price-maps)])
                               all-sequences)))))

(defn result
  []
  (let [sns (parse (input))
        prs-n-cns (map #(take 2000 %) (map prices-and-changes sns))
        price-maps (map price-map prs-n-cns)
        [_ most-bananas] (best-sequence price-maps)]
    [(reduce + (map #(nth (iterate evolve %) 2000) sns))
     most-bananas]))
