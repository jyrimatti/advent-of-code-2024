(ns advent-of-code-2024.day17
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(set! *unchecked-math* true)

(defn input
  []
  (-> "day17.txt"
      io/resource
      io/file
      slurp))

(defn parse
  [input]
  (let [lines (vec (.split input "\n"))
        regs (mapv #(Integer/parseInt %) (map #(.replaceAll % "[^0-9]" "") (take 3 lines)))]
    {:A (regs 0)
     :B (regs 1)
     :C (regs 2)
     :prog (vec (map parse-long (.split (.replaceAll (lines 4) "Program: " "") ",")))
     :out []}))

(defn f0
  [^long operand ^long A]
  [nil (bit-shift-right A operand)])

(defn f1
  [^long operand ^long B]
  [nil nil (bit-xor B operand)])

(defn f2
  [^long operand]
  [nil nil (bit-and 2r111 operand)])

(defn f3
  [^long operand ^long A]
  (if (== 0 A)
    [nil]
    [operand]))

(defn f4
  [^long B ^long C]
  [nil nil (bit-xor B C)])

(defn f5
  [^long operand out]
  [nil nil nil nil (conj out (bit-and 2r111 operand))])

(defn f6
  [^long operand ^long A]
  [nil nil (bit-shift-right A operand)])

(defn f7
  [^long operand ^long A]
  [nil nil nil (bit-shift-right A operand)])

(defn combo ^long
  [^long A ^long B ^long C ^long x]
  (case x
    0 0
    1 1
    2 2
    3 3
    4 A
    5 B
    6 C))

(defn calc ^bytes
  [ip_ init-A init-B init-C prog expected-out]
  (loop [ip ip_
         A init-A
         B init-B
         C init-C
         out []
         out-changed false]
    (if (and (< ip (count prog))
             (or (nil? expected-out) (not out-changed) (= out (subvec expected-out 0 (count out)))))
      (let [opcode (prog ip)
            ip-next (unchecked-inc ip)
            [^long ip_ ^long A_ ^long B_ ^long C_ out_] (case opcode
                 0 (f0 (combo A B C (prog ip-next)) A)
                 1 (f1              (prog ip-next) B)
                 2 (f2 (combo A B C (prog ip-next)))
                 3 (f3              (prog ip-next) A)
                 4 (f4  B C)
                 5 (f5 (combo A B C (prog ip-next)) out)
                 6 (f6 (combo A B C (prog ip-next)) A)
                 7 (f7 (combo A B C (prog ip-next)) A))]
        (recur (if (or (nil? ip_) (== ip_ ip))
                 (unchecked-inc ip-next) ip_) (or A_ A) (or B_ B) (or C_ C) (or out_ out) (== opcode 5)))
      out)))

(defn result
  []
  (let [computer (parse (input))
        prog (:prog computer)
        A (computer :A)
        B (computer :B)
        C (computer :C)
        part1 (calc 0 A B C prog nil)
        part2 (drop-while #(not= prog (second %))
                          (map (fn [x] [x (calc 0 x B C prog prog)])
                               (map #(+ (bit-shift-left % 16) 15375) (range))))
        ]
    [(string/join "," part1)
     (first (first part2))]))
