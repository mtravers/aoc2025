(ns aoc2025.day5
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.math :as mu]
            [aoc2025.utils :as au]
            [clojure.string :as str]))

;;; ⤰⤯⤰ Data ⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯

(def example
  ["3-5"
   "10-14"
   "16-20"
   "12-18"
   ""
   "1"
   "5"
   "8"
   "11"
   "17"
   "32"]
  )

(defn read-data
  [lines]
  (let [break (u/position= "" lines)
        [ranges available] (split-at break lines)
        ranges (map (fn [p] (mapv u/coerce-numeric (str/split p #"-"))) ranges)
        available (map u/coerce-numeric (rest available))]
    [ranges available]))

(def data (ju/file-lines "~/Downloads/aoc-day5.txt"))

;;; ⤰⤯⤰ Part 1 ⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯

(defn p1
  [data]
  (let [[ranges avail] (read-data data)]
    (count (filter (fn [a] (some (fn [[r0 r1]]
                                   (<= r0 a r1))
                                 ranges))
                   avail))))


;;; ⤰⤯⤰ Part 2 ⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯⤰⤯

;;; Assumes a certain ordering
(defn combine-ranges
  [[r0 r1] [s0 s1]]
  (assert (<= r0 s0 r1))
  [r0 (max r1 s1)])

(defn replace-element
  [seq new old]
  (replace {old new} seq))

(defn delete-element
  [seq elt]
  (vec (u/remove= elt seq)))

(defn add-range
  [com-ranges [r0 r1 :as r]]
  (let [[s0 s1 :as s] (u/some-thing (fn [[s0 s1]] (<= r0 s1)) com-ranges)]
    ;; s1 > r0
    (cond

      (nil? s)                          ;no s found, tack onto end
      (conj com-ranges r)

      ;; We know (< r0 s1)

      (= r1 s0)
      (add-range (delete-element com-ranges s) [r0 s1])

      ;; non-overlapping so insert new one (A)
      (< r1 s0)                         ; (<= r0 r1 s0 s1)
      (vec (u/insert-before com-ranges r s))

      ;; overlapping (B)
      (<= r0 s0 r1 s1)                         ; 
      (add-range (delete-element com-ranges s) (combine-ranges r s))

      ;; contained (C)
      (<= r0 s0 s1 r1)                         ;(< r0 s0 s1 r1)
      (replace-element com-ranges r s)

      ;;
      (<= s0 r0 r1 s1)
      com-ranges

      (<= s0 r0 s1 r1)
      (add-range (delete-element com-ranges s) (combine-ranges s r))
      
      :else
      (throw (ex-info "foo" {:s s :r r}))

      )))


(defmacro tc [form result]
  `(do (prn :testing '~form)
       (assert (= ~result ~form))))

(defn test-add-range
  []
  (tc (add-range [[1 2] [5 10]] [3 4])
      [[1 2] [3 4] [5 10]])
  (tc (add-range [[1 2] [5 10]] [-1 0])
      [[-1 0] [1 2] [5 10]])
  (tc (add-range [[1 2] [5 10]] [20 30])
      [[1 2] [5 10] [20 30]])
  (tc (add-range [[1 20] [30 40]] [2 8])
      [[1 20] [30 40]])
  (tc (add-range [[1 20] [30 40]] [10 22])
      [[1 22] [30 40]])
  (tc (add-range [[10 20] [30 40]] [5 15])
      [[5 20] [30 40]])

  (tc (add-range [[10 20] [30 40]] [25 26])
      [[10 20] [25 26] [30 40]])
  (tc (add-range [[10 20] [30 40]] [50 60])
      [[10 20] [30 40] [50 60]])

  (tc (add-range [[10 20] [30 40]] [15 35])
      [[10 40]])                        ;Way to go on this one

  ;; Now for the fenceposts
  (tc (add-range [[0 5] [10 20]] [5 7])
      [[0 7] [10 20]])

  (tc (add-range [[0 5] [10 20]] [7 10])
      [[0 5] [7 20]])
  )



(defn p2
  [data]
  (let [[ranges _] (read-data data)
        com-ranges
        (loop [com-ranges []                ;an ordered set of non-overlapping ranges
               ranges ranges]
          (if (empty? ranges)
            com-ranges
            (recur (add-range com-ranges (first ranges))
                   (rest ranges))))]
    (reduce +
            (map (fn [[r0 r1]] (inc (- r1 r0)))
                 com-ranges))))
        
