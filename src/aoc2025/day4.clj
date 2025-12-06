(ns aoc2025.day4
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.math :as mu]
            [aoc2025.utils :as au]
            [clojure.string :as str]))


;;; ◩◪◩ Data ◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪

(def example
  ["..@@.@@@@."
   "@@@.@.@.@@"
   "@@@@@.@.@@"
   "@.@@@@..@."
   "@@.@@@@.@@"
   ".@@@@@@@.@"
   ".@.@.@.@@@"
   "@.@@@.@@@@"
   ".@@@@@@@@."
   "@.@.@@@.@."])

(def data
  (vec (ju/file-lines "~/Downloads/aoc-day4.txt")))

;;; ◩◪◩ Part 1 ◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪

(defn free?
  [grid p]
  (and (= \@ (au/rget grid p))
       (< (count (filter (fn [n] (= \@ (au/rget grid n)))
                         (au/neighbors-d grid p)))
          4)))

(defn p1
  [grid]
  (count (filter (partial free? grid) (au/all-points grid))))

;;; ◩◪◩ Part 2 ◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪◩◪

(defn p2
  [grid]
  (let [removes (filter (partial free? grid) (au/all-points grid))]
    (if (empty? removes)
      0
      (+ (count removes)
         (p2 (au/rset* grid removes \.))))))

;;; Some au/r... fns don't work on vectors of strings, so need to convertn
(comment
  (p2 (mapv vec data)))
