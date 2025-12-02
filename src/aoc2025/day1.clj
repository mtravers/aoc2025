(ns aoc2025.day1
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [clojure.string :as str]))

(def input (->> (ju/file-lines "~/Downloads/advent1.txt")
                (map #(* (u/coerce-numeric (subs % 1)) (if (= (first %) \L) -1 1)))))

(defn p1
  [data]
  (loop [[h & rest] data
         pos 50
         count 0]
    (if (nil? h)
      count
      (let [npos (mod (+ pos h) 100)]
        (if (zero? npos)
          (recur rest 0 (inc count))
          (recur rest npos count))))))

(defn zeros
  [start move]
  (if (pos? move)
    (quot (+ start move) 100)
    (if (zero? start)
      (quot (+ start (- move)) 100)
      (- (quot (+ start move -100) 100)))))    ;not sure


(defn p2
  [data]
  (loop [[h & rest] data
         pos 50
         count 0]
    (if (nil? h)
      count
      (let [npos (mod (+ pos h) 100)
            zeros (zeros pos h)]
        #_ (prn :move h npos zeros)
        (recur rest npos (+ count zeros))))))
    
    
