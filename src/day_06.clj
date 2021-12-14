(ns day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def seed
  [3 4 3 1 2])

(def final-input
  (-> "day-06.txt"
      io/resource
      slurp
      (str/split #",")
      (->> (map str/trim)
           (map #(Integer/parseInt %))))
  )

(defn next-round
  [[seeds n-rounds]]
  (let [bursting (filter (partial = 0) seeds)
        others (filter (partial not= 0) seeds)
        next-gen (concat (repeat (count bursting) 6)
                         (repeat (count bursting) 8)
                         (map dec others)
                         )]
    (if (= 1 n-rounds)
      (reduced next-gen)
      [next-gen (dec n-rounds)]
      )))

(defn simulate
  [seeds n-rounds]
  (->> [seeds n-rounds]
       (iterate next-round)
       (drop-while (complement reduced?))
       first
       deref))

(simulate seed 80)
(assert (= 5934 (count (simulate seed 80))))
(count (simulate final-input 80))
;; => 360610

(assert (= 26984457539 (count (simulate seed 256))))
(count (simulate final-input 256))
