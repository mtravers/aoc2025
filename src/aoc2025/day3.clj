(ns aoc2025.day3
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.math :as mu]
            [clojure.string :as str]))


;;; ⦿|⦾| Data |⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾

(def t1
  ["987654321111111"
   "811111111111119"
   "234234234234278"
   "818181911112111"])

(def t1d (map (fn [l] (mapv #(- (int %) (int \0)) (seq l)))
              t1))


(def data
  (map (fn [l] (mapv #(- (int %) (int \0)) (seq l)))
       (ju/file-lines "~/Downloads/aoc-day3.txt")))


;;; ⦿|⦾| Part 1 |⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾


;;; TODO variant of u/max-by, maybe → 
(defn max-by-first "Find the maximum element of `seq` based on keyfn"
  [keyfn seq]
  (when-not (empty? seq)
    (reduce (fn [a b] (if (u/>=* (keyfn a) (keyfn b)) a b))
            seq)))

(defn max-joltage
  [row]
  (let [p0 (max-by-first #(get row %) (range 0 (dec (count row))))
        p1 (u/max-by #(get row %) (range (inc p0) (count row)))]
    (+ (* 10 (get row p0))
       (get row p1))))

(defn p1
  [data]
  (reduce + (map max-joltage data)))

;;; ⦿|⦾| Part 2|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾-⦿|⦾|⦿|⦾

(defn pow10
  [i]
  (long (Math/pow 10 i)))

(defn max-joltage2
  ([row n i]
   (if (= n 0)
     0
     (let [p (max-by-first #(get row %) (range i (- (count row) (dec n))))]
       (+ (* (get row p) (pow10 (dec n)))
             (max-joltage2 row (dec n) (inc p))))))
  ([row]
   (max-joltage2 row 12 0)))

(defn p2
  [data]
  (reduce + (map max-joltage2 data)))
