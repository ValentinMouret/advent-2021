(ns day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-input
  [199
   200
   208
   210
   200
   207
   240
   269
   260
   263])

(def final-input
  (->> "day-01.txt"
     io/resource
     slurp
     str/split-lines
     (map #(Integer/parseInt %))))

(defn increases
  [input]
  (map > (rest input) input))

(defn part-1
  [input]
  (->> input
       increases
       (filter identity)
       count))

(assert (= 7 (part-1 test-input)))
(part-1 final-input)
;; => 1342


(defn part-2
  [input]
  (part-1 (map +
               (rest (rest input))
               (rest input)
               input)))

(assert (= 5 (part-2 test-input)))
(part-2 final-input)
;; => 1378
