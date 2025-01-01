(ns advent-of-code-2024.core-test
  (:require [clojure.test :refer [deftest is]]
            [advent-of-code-2024.day01 :as Day01]
            [advent-of-code-2024.day02 :as Day02]
            [advent-of-code-2024.day03 :as Day03]
            [advent-of-code-2024.day04 :as Day04]
            [advent-of-code-2024.day05 :as Day05]
            [advent-of-code-2024.day06 :as Day06]
            [advent-of-code-2024.day07 :as Day07]
            [advent-of-code-2024.day08 :as Day08]
            [advent-of-code-2024.day09 :as Day09]
            [advent-of-code-2024.day10 :as Day10]
            [advent-of-code-2024.day11 :as Day11]
            [advent-of-code-2024.day12 :as Day12]
            [advent-of-code-2024.day13 :as Day13]
            [advent-of-code-2024.day14 :as Day14]
            [advent-of-code-2024.day15 :as Day15]
            [advent-of-code-2024.day16 :as Day16]
            [advent-of-code-2024.day17 :as Day17]
            [advent-of-code-2024.day18 :as Day18]
            [advent-of-code-2024.day19 :as Day19]
            [advent-of-code-2024.day20 :as Day20]
            [advent-of-code-2024.day21 :as Day21]
            [advent-of-code-2024.day22 :as Day22]
            [advent-of-code-2024.day23 :as Day23]
            [advent-of-code-2024.day24 :as Day24]
            [advent-of-code-2024.day25 :as Day25]))

(defmacro dotest [name expr]
    `(let [start# (. System (nanoTime))
           return# ~expr]
       (println (str ~name ": " (/ (double (- (. System (nanoTime)) start#)) 1000000000.0) " secs"))
       return#))

(deftest all-days
  (dotest "Day01" (is (= [1765812 20520794] (Day01/result))))
  (dotest "Day02" (is (= [598 634] (Day02/result))))
  (dotest "Day03" (is (= [181345830 98729041] (Day03/result)))) 
  (dotest "Day04" (is (= [2370 1908] (Day04/result))))
  (dotest "Day05" (is (= [6949 4145] (Day05/result))))
  (dotest "Day06" (is (= [5030 1928] (Day06/result))))
  (dotest "Day07" (is (= [4364915411363 38322057216320] (Day07/result))))
  (dotest "Day08" (is (= [291 1015] (Day08/result))))
  (dotest "Day09" (is (= [6356833654075 6389911791746] (Day09/result))))
  (dotest "Day10" (is (= [517 1116] (Day10/result))))
  (dotest "Day11" (is (= [204022 241651071960597] (Day11/result))))
  (dotest "Day12" (is (= [1456082 872382] (Day12/result))))
  (dotest "Day13" (is (= [29436 103729094227877] (Day13/result))))
  (dotest "Day14" (is (= [230172768 8087] (Day14/result))))
  (dotest "Day15" (is (= [1412971 1429299] (Day15/result))))
  (dotest "Day16" (is (= [95444 513] (Day16/result))))
  (dotest "Day17" (is (= ["7,1,3,7,5,1,0,3,4" 190384113204239] (Day17/result))))
  (dotest "Day18" (is (= [298 "52,32"] (Day18/result))))
  (dotest "Day19" (is (= [247 692596560138745] (Day19/result))))
  (dotest "Day20" (is (= [1369 979012] (Day20/result))))
  (dotest "Day21" (is (= [215374 260586897262600] (Day21/result))))
  (dotest "Day22" (is (= [17724064040 1998] (Day22/result))))
  (dotest "Day23" (is (= [1314 "bg,bu,ce,ga,hw,jw,nf,nt,ox,tj,uu,vk,wp"] (Day23/result))))
  (dotest "Day24" (is (= [48806532300520 "ddn,kqh,nhs,nnf,wrc,z09,z20,z34"] (Day24/result))))
  (dotest "Day25" (is (= [3107] (Day25/result))))
  )
