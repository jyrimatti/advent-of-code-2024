(ns advent-of-code-2024.day24
  (:require
   [clojure.java.io :as io]
   [clojure.math :refer [pow]]
   [clojure.string :refer [join]]))

(defn input
  []
  (-> "day24.txt"
      io/resource
      io/file
      slurp))

(defn parse-input
  [i]
  (let [[a b] (.split i ": ")]
    [a (parse-long b)]))

(defn parse-connection
  [c]
  (let [[[_ l op r out]] (re-seq #"([^ ]+) (AND|OR|XOR) ([^ ]+) -> ([^ ]+)" c)]
    {:left l :right r :op op :out out}))

(defn parse
  [input]
  (let [lines (.split input "\n")
        [inputs connections] (split-with (comp not empty?) lines)]
    [(into {} (map parse-input inputs))
     (mapv parse-connection (next connections))]))

(defn try-resolve
  [wires conn]
  (if (contains? wires (conn :out))
    wires
    (if (and (contains? wires (conn :left))
             (contains? wires (conn :right)))
      (assoc wires
              (conn :out)
              (case (conn :op)
                "AND" (bit-and (wires (conn :left)) (wires (conn :right)))
                "OR"  (bit-or  (wires (conn :left)) (wires (conn :right)))
                "XOR" (bit-xor (wires (conn :left)) (wires (conn :right)))))
      wires)))

(defn solve
  [wires conns]
  (let [new-wires (reduce try-resolve wires conns)
        new-conns (filter #(not (contains? new-wires (% :out))) conns)]
    (if (= new-wires wires)
      new-wires
      (if (empty? new-conns)
        new-wires
        (recur new-wires new-conns)))))

(defn to-dec
  [n bits]
  (if (empty? bits)
    0
    (+ (* (first bits) (pow 2 n)) (to-dec (inc n) (next bits)))))

(defn to-decimal
  [wires prefix]
  (long (to-dec 0 (map second (sort (filter #(.startsWith (first %) prefix) wires))))))

(defn find-by-out
  [conns out]
  (first (filter #(= out (% :out)) conns)))

(defn is-of-op
  [op num conn]
  (let [numstr (str (if (< num 10) "0" "") num)]
    (or (and (not (nil? conn))
             (= (conn :op) op)
             (= (conn :left) (str "x" numstr))
             (= (conn :right) (str "y" numstr)))
        (and (not (nil? conn))
             (= (conn :op) op)
             (= (conn :left) (str "y" numstr))
             (= (conn :right) (str "x" numstr))))))

(declare is-carry-of)

(defn is-mid-of
  [conns num conn]
  (and (= (conn :op) "AND")
       (or (and (is-of-op "XOR" num (find-by-out conns (conn :left)))
                (is-carry-of conns (dec num) (find-by-out conns (conn :right))))
           (and (is-of-op "XOR" num (find-by-out conns (conn :right)))
                (is-carry-of conns (dec num) (find-by-out conns (conn :left)))))))

(defn is-carry-of
  [conns num conn]
  (or (is-of-op "AND" 0 conn)
      (and (= (conn :op) "OR")
           (or
            (and (is-of-op "AND" num (find-by-out conns (conn :left)))
                 (is-mid-of conns num (find-by-out conns (conn :right))))
            (and (is-of-op "AND" num (find-by-out conns (conn :right)))
                 (is-mid-of conns num (find-by-out conns (conn :left))))))))

(defn is-of-xy
  [op conn]
  (or
   (and (= (conn :op) op)
        (.startsWith (conn :left) "x")
        (.startsWith (conn :right) "y"))
   (and (= (conn :op) op)
        (.startsWith (conn :left) "y")
        (.startsWith (conn :right) "x"))))

(defn not-xor-outputs
  [conns]
  (filter #(not= "XOR" (% :op)) (drop-last (sort-by :out (filter #(.startsWith (% :out) "z") conns)))))

(defn xors-not-inputs-or-outputs
  [conns]
  (filter #(and (not (.startsWith (% :out) "z"))
                (not (is-of-xy "XOR" %)))
          (filter #(= (% :op) "XOR") conns)))

(defn x-xor-y-not-point-to-xor-and-and
  [conns]
  (filter #(not= #{"AND" "XOR"} (set (map :op (filter (fn [c] (or (= (c :left) (% :out)) (= (c :right) (% :out)))) conns))))
          (filter #(not (is-of-op "XOR" 0 %))
                  (filter #(is-of-xy "XOR" %) conns))))

(defn x-and-y-not-point-to-or
  [conns]
  (filter #(not= #{"OR"} (set (map :op (filter (fn [c] (or (= (c :left) (% :out)) (= (c :right) (% :out)))) conns))))
          (filter #(not (is-of-op "AND" 0 %))
                  (filter #(is-of-xy "AND" %) conns))))

(defn invalid
  [conns]
  (distinct (concat (not-xor-outputs conns)
                    (x-xor-y-not-point-to-xor-and-and conns)
                    (xors-not-inputs-or-outputs conns)
                    (x-and-y-not-point-to-or conns))))

(defn result
  []
  (let [[initial-wires initial-conns] (parse (input))
        invalids (invalid initial-conns)]
        
    [(to-decimal (solve initial-wires initial-conns) "z")
     (join "," (sort (map :out invalids)))]))
