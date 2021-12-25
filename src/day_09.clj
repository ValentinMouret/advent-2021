(ns day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input
  [[2 1 9 9 9 4 3 2 1 0]
   [3 9 8 7 8 9 4 9 2 1]
   [9 8 5 6 7 8 9 8 9 2]
   [8 7 6 7 8 9 6 7 8 9]
   [9 8 9 9 9 6 5 6 7 8]])

(def final-input
  (-> "day-09.txt"
      io/resource
      slurp
      (str/split #"\R")
      (->> (mapv #(str/split % #""))
           (mapv (fn [row] (mapv #(Long/parseLong %) row))))))

(defn neighbors
  [grid [x y]]
  (for [dx    '(-1 1 0)
        dy    '(-1 1 0)
        :when (not= (Math/abs dx) (Math/abs dy))]
    (-> grid
        (nth (+ x dx) [])
        (nth (+ y dy) nil))))

(defn part-1
  [input]
  (let [mins (for [x (range (count input))
                   y (range (count (first input)))]
               (let [neighbors (neighbors input [x y])
                     cell      (nth (nth input x) y)
                     is-min    (< cell (apply min (filter (complement nil?) neighbors)))]
                 (if is-min (inc cell) 0)))]
    (reduce + (flatten mins))))

(assert (= 15 (part-1 demo-input)))
(part-1 final-input)
;; => 491
