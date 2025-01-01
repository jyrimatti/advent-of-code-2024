(defproject advent-of-code-2024 "0.1.0-SNAPSHOT"
  :description "Advent of code 2024"
  :url "http://github.com/advent-of-code-2024"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [astar-search "0.2"]
                 [trie/trie "0.1.1"]]
  :jvm-opts ["-Xmx12g"]
  :repl-options {
                 :init-ns advent-of-code-2024.day01
                 })
