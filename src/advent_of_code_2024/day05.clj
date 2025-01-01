(ns advent-of-code-2024.day05
  (:require
   [clojure.java.io :as io]))

(defn input
  []
  (-> "day05.txt"
      io/resource
      io/file
      slurp))

(defn valid?
  [pages rule]
  (let [[before after] (map #(.indexOf pages %) rule)]
    (or (< before after) (== before -1) (== after -1))))

(defn correct?
  [rulemap pages]
  (every? #(valid? pages %) rulemap))

(defn middle
  [pages]
  (.get pages (/ (count pages) 2)))

(defn fix-order
  [rulemap pages]
  (let [[before after] (first (filter #(not (valid? pages %)) rulemap))
        stripped (filterv #(not= before %) pages)
        divider (.indexOf stripped after)
        fixed (vec (concat (take divider stripped) [before] (drop divider stripped)))]
    (if (correct? rulemap fixed)
      fixed
      (recur rulemap fixed))))

(defn result
  []
  (let [rows (.split (input) "\n")
        rules (take-while #(not-empty %) rows)
        pages (rest (drop-while #(not-empty %) rows))
        rulemap (mapv (comp #(map parse-long %) rest) (mapcat #(re-seq #"(\d+)\|(\d+)" %) rules))
        pagesvec (mapv #(map parse-long %) (map #(.split % ",") pages))
        correctly (filter #(correct? rulemap %) pagesvec)
        incorrectly (filter #(not (correct? rulemap %)) pagesvec)]
  [(reduce + (map middle correctly))
   (reduce + (map (comp middle (partial fix-order rulemap)) incorrectly))]))
