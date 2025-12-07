(ns aoc2025.day2
  (:require [hyperphor.multitool.core :as u]
            [hyperphor.multitool.cljcore :as ju]
            [hyperphor.multitool.math :as mu]
            [clojure.string :as str]))

(def raw-input
  "492410748-492568208,246-390,49-90,16-33,142410-276301,54304-107961,12792-24543,3434259704-3434457648,848156-886303,152-223,1303-1870,8400386-8519049,89742532-89811632,535853-567216,6608885-6724046,1985013826-1985207678,585591-731454,1-13,12067202-12233567,6533-10235,6259999-6321337,908315-972306,831-1296,406-824,769293-785465,3862-5652,26439-45395,95-136,747698990-747770821,984992-1022864,34-47,360832-469125,277865-333851,2281-3344,2841977-2953689,29330524-29523460")

(def input
  (map (fn [p] (mapv u/coerce-numeric (str/split p #"-")))
       (str/split raw-input #",")))

;;; Longs go up to 10^18
(defn pow10
  [i]
  (long (Math/pow 10 i)))

(defn double-prefix
  ([prefix exp]
   (+ (* prefix exp) prefix))
  ([prefix]
   (u/coerce-numeric (str prefix prefix))))


(defn count-i
  [from to]
  (loop [prefix from
         count 0]
    (if (<= (double-prefix prefix) to)
      (recur (inc prefix) (inc count))
      count)))
    

(defn invalids
  [[r0 r1]]
  (let [r0l (inc (int (Math/floor (Math/log10 r0)))) ;log
        r0s (str r0)]                                ;str
    (prn :r0l r0l :ros (count r0s))
    (if (odd? r0l)                      ;odd digits, go up to next pow10
      (invalids [(pow10 r0l) r1])
      ;; even digits
      (let [exp (pow10 (/ r0l 2))
            prefix (quot r0 exp)
            doubled (double-prefix prefix exp)]
        (prn :doubled doubled exp)
        (if (<= r0 doubled)
          (if (> doubled r1)
            0
            (count-i prefix r1))
          ;; doubled < r0, so we increment and try again
          (invalids [(+ (* (inc prefix) exp) (inc prefix)) r1]))))))





(def t1
  [[11 22]
   [95 115]
   [998 1012]
   [1188511880 1188511890]
   [222220 222224]
   [1698522 1698528]
   [446443 446449]
   [38593856 38593862]]  )

(comment
  (zipmap t1 (map invalids t1)))

;;; Oh I totally misunderstood this! Fuck!


(defn invalids-0
  [from to]
  (loop [prefix from
         invalids []]
    (let [x (double-prefix prefix)]
      (if (<= x to)
        (recur (inc prefix) (conj invalids x))
        invalids))))

(defn invalids
  [[r0 r1]]
  (let [r0l (inc (int (Math/floor (Math/log10 r0)))) ;log
        r0s (str r0)]                                ;str
    (prn :r0l r0l :ros (count r0s))
    (if (odd? r0l)                      ;odd digits, go up to next pow10
      (invalids [(pow10 r0l) r1])
      ;; even digits
      (let [exp (pow10 (/ r0l 2))
            prefix (quot r0 exp)
            doubled (double-prefix prefix exp)]
        (prn :doubled doubled exp)
        (if (<= r0 doubled)
          (if (> doubled r1)
            []
            (invalids-0 prefix r1))
          ;; doubled < r0, so we increment and try again
          (invalids [(+ (* (inc prefix) exp) (inc prefix)) r1]))))))


(defn p1
  [input]
  (reduce + (mapcat invalids input)))


;;; ⊥⊥⊤ Part 2 ⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤⊥⊥⊤⊤

;;; Brute forcing works

;;; Now in Multitool
(defn divisors
  [n]
  (filter (partial mu/divides? n) (range 1 n)))

(defn invalid?
  [n]
  (let [s (str n)
        l (count s)]
    (some (fn [i]
            (let [prefix (subs s 0 i)]
              (every? (fn [j] (= prefix (subs s j (+ j i))))
                      (range i l i))))
          (divisors l))))
            
(defn invalids2
  [[r0 r1]]
  (filter invalid? (range r0 (inc r1))))

(comment (zipmap t1 (map invalids2 t1)))

(defn p2
  [input]
  (reduce + (mapcat invalids2 input)))
