(ns aoc2025.day8
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.math :as mu]
            [aoc2025.utils :as au]
            [clojure.string :as str]
            [clojure.set :as set]))

;;; [⌑] Data [⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑]

(def data
  (mapv #(mapv u/coerce-numeric (str/split % #","))
        (ju/file-lines "~/Downloads/aoc-day8.txt")))

(def distance-pairs
  (sort-by
   second
   (for [i (range (count data))
         j (range (inc i) (count data))]
     [[i j] (mu/euclidean-distance (get data i) (get data j))])))

;;; [⌑] Part 1 [⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑]

(defn p1-groups
  []
  (loop [index {}
         [[p0 p1] & tail] (take 1000 (map first distance-pairs))]
    (let [s0 (or (get index p0) #{p0})
          s1 (or (get index p1) #{p1})
          sp (set/union s0 s1)
          nindex (reduce (fn [indx elt] (assoc indx elt sp))
                         index
                         sp)]
      (if (empty? tail)
        (distinct (vals nindex))
        (recur nindex tail)))))

(defn p1
  []
  (apply * (take 3 (sort > (map count (p1-groups))))))
      
;;; [⌑] Part 2 [⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑][⌑]

(defn p2-last
  []
  (loop [index {}
         last nil
         [[p0 p1] & tail] (map first distance-pairs)]
    (let [s0 (or (get index p0) #{p0})
          s1 (or (get index p1) #{p1})
          sp (set/union s0 s1)
          nindex (reduce (fn [indx elt] (assoc indx elt sp))
                         index
                         sp)]
      (if (and (= 1 (count (distinct (vals nindex))))
               (= 1000 (count (first (vals nindex)))))
        [p0 p1]
        (recur nindex [p0 p1] tail)))))

(defn p2
  []
  (let [[p0 p1] (p2-last)]
    (* (first (get data p0))
       (first (get data p1)))))




            
  

