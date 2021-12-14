(ns day-07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input
  "16,1,2,0,4,2,7,1,2,14")


(defn parse-input
  [input]
  (-> input
      (str/split #",")
      (->> (map str/trim)
           (map #(Long/parseLong %)))))

(def final-input
  (-> "day-07.txt"
      io/resource
      slurp
      parse-input))

(defn naive-search
  [points]
  (let [mini (apply min points)
        maxi (apply max points)]
    (->> (range mini maxi)
         (map (fn [x0] [x0 (reduce + (map #(Math/abs (- x0 %))  points))]))
         (sort-by second)
         first)))

(defn part-1
  [points]
  (first (naive-search points)))

(assert (= 2 (part-1 (parse-input test-input))))
(naive-search final-input)
;; => [337 355521]


(defn naive-search-2
  [points]
  (let [mini (apply min points)
        maxi (apply max points)]
    (->> (range mini maxi)
         (map (fn [x0]
                (let [distances (map #(Math/abs (- x0 %))  points)
                      fuel-cost (map #(/ (* % (inc %)) 2) distances)]
                  [x0 (reduce + fuel-cost)])))
         (sort-by second)
         first)))

(naive-search-2 (parse-input test-input))
(naive-search-2 final-input)
;; => [493 100148777]
