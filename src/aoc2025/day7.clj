(ns aoc2025.day7
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.math :as mu]
            [aoc2025.utils :as au]
            [clojure.string :as str]))

(def example
  [".......S......."
   "..............."
   ".......^......."
   "..............."
   "......^.^......"
   "..............."
   ".....^.^.^....."
   "..............."
   "....^.^...^...."
   "..............."
   "...^.^...^.^..."
   "..............."
   "..^...^.....^.."
   "..............."
   ".^.^.^.^.^...^."
   "..............."]
  )

(def data (ju/file-lines "~/Downloads/aoc-day7.txt"))


;;; ❖⟐❖ Part 1 ❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖

(defn assoc-if
  [coll key val]
  (if (contains? coll key)
    (assoc coll key val)
    coll))

(defn update-if
  [coll key f & args]
  (if (contains? coll key)
    (apply update coll key f args)
    coll))

(defn propagate
  [state new]
  (reduce (fn [nstate i]
            (if (and (= \^ (get new i))
                     (or (= \| (get nstate i))
                         (= \S (get nstate i))))
              (-> nstate
                  (assoc-if (dec i) \|)
                  (assoc-if i \.)
                  (assoc-if (inc i) \|))
              nstate))
          state
          (range (count state))))

(defn p0
  [data]
  (reduce propagate (map vec data)))

(defn p1
  [data]
  (let [data (map vec
                  (cons (str/replace (first data) \S \|)
                        (rest data)))
        counter (atom 0)
        end
        (reduce (fn [state new]
                  (swap! counter + (count (u/mapf (fn [i]
                                                    (and (= \^ (get new i))
                                                         (= \| (get state i))))
                                                  (range (count state)))))
                  (propagate state new))
                data)]
    @counter))

;;; ❖⟐❖ Part 2 ❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖⟐❖

;;; State here is a vector of path counts
(defn propagate2
  [state new]
  (reduce (fn [nstate i]
            (if (= \^ (get new i))
              (-> nstate
                  (update-if (dec i) + (get state i))
                  (assoc i 0)
                  (update-if (inc i) + (get state i)))
              nstate))
          state
          (range (count state))))

(defn p2
  [data]
  (->> (reduce propagate2 
               (mapv #(if (= % \S) 1 0) (first data))
               (vec (rest data)))
       (reduce +)))
