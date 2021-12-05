(ns day-05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn parse-input
  [input]
  (->> (str/split input #"\R")
       (map #(re-find #"([0-9]*),([0-9]*) -> ([0-9]*),([0-9]*)" %))
       (map rest)
       (map (fn [nums] (map #(Integer/parseInt %) nums)))
       (map (fn [[x1 y1 x2 y2]] [[x1 y1] [x2 y2]]))))

(def final-input
  (-> "day-05.txt"
      io/resource
      slurp
      parse-input))

(defn get-line
  [[x1 y1] [x2 y2]]
  (cond (= x1 x2) (let [[ymin ymax] (sort [y1 y2])]
                    (for [y (range ymin (inc ymax))]
                      [x1 y]))
        (= y1 y2) (let [[xmin xmax] (sort [x1 x2])]
                    (for [x (range xmin (inc xmax))]
                      [x y1]))
        :else (let [dx (- x1 x2)
                    dy (- y1 y2)]
                (for [d (range (inc (Math/abs dx)))]
                  [(- x1 (* d (if (pos? dx) 1 -1)))
                   (- y1 (* d (if (pos? dy) 1 -1)))]))))

(defn find-intersections
  [points]
  (->> points
       flatten
       (partition 2)
       (group-by identity)
       (map (comp count second))
       (filter #(> % 1))
       count))

(defn part-1
  [points]
  (->> points
       (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))))
       (map (partial apply get-line))
       find-intersections))

(assert (= 5 (part-1 (parse-input test-input))))
(part-1 final-input)
;; => 7142

(defn part-2
  [points]
  (->> points
       (map (partial apply get-line))
       find-intersections))
(part-2 (parse-input test-input))
(part-2 final-input)
;; => 20012
