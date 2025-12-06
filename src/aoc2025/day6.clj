(ns aoc2025.day6
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.math :as mu]
            [aoc2025.utils :as au]
            [clojure.string :as str]))


;;; ⩌⩍⩌ Part 1 ⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍

(def data (map #(map u/coerce-numeric (str/split % #"\s+"))
               (ju/file-lines "~/Downloads/aoc-day6.txt")))

(defn p1
  [data]
  (let [nums (drop-last 1 data)
        ops (last data)]
    (reduce + (for [i (range (count ops))]
                (reduce (if (= (nth ops i) "*") * +)
                        (map #(nth % i) nums) )))))
          
;;; ⩌⩍⩌ Part 2 ⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍⩌⩍

(def data2 (ju/file-lines "~/Downloads/aoc-day6.txt"))

(def example
  ["123 328  51 64 "
   " 45 64  387 23 "
   "  6 98  215 314"
   "*   +   *   +  "])

(defn p2
  [data]
  (let [columns
        (->> data
             u/transpose
             (map #(apply str %))
             (map str/trim)
             (u/partition-if empty?))]
    (reduce +
            (map (fn [col]
                   (let [[head & tail] (if (empty? (first col)) (rest col) col)
                         op (if (= (last head) \+) + *)
                         nums (map (comp u/coerce-numeric str/trim) (cons (subs head 0 (dec (count head))) tail))]
                     (apply op nums)))
                 columns))))
