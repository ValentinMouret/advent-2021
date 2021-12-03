(ns day-03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(def final-input
  (-> "day-03.txt"
      io/resource
      slurp
      str/split-lines))

(defn parse-input
  [line]
  (let [split (str/split line #"")]
    (map #(Integer/parseInt %) split)))

(defn gamma-rate
  [values]
  (let [start (repeat (count (first values)) 0)
        as-list (->> values
                     (reduce (fn [acc new] (map + acc new)) start)
                     (map (fn [sum] (if (> sum (/ (count values) 2))
                                      1
                                      0)))
                     )]
    (str/join as-list)))

;; Yes, one could easily run the "not" bit operation,
;; but somehow it was a PITA because of Java not having
;; unsigned bytes. wtf.
(defn epsilon-rate
  [gamma-rate]
  (str/join (map #(if (= \1 %) \0 \1) gamma-rate)))

(defn part-1
  [inputs]
  (let [gamma (gamma-rate inputs)
        epsilon (epsilon-rate gamma)]
    (* (Integer/parseInt gamma 2)
       (Integer/parseInt epsilon 2))))

(assert (= 198 (part-1 (map parse-input test-input))))
(part-1 (map parse-input final-input))
;; => 3320834

(defn search
  [inputs freq-fn]
  (loop [table inputs
         pos 0]
    (let [freqs (->> table
                     (map #(nth % pos))
                     frequencies
                     (sort-by second)
                     reverse)
          elected (freq-fn freqs)
          remaining (->> table
                         (filter #(= elected (nth % pos))))]
      (if (= 1 (count remaining))
        (str/join (first remaining))
        (recur remaining (inc pos))))))


(defn search-ox
  [inputs]
  (search inputs (fn [[most-freq least-freq]]
                   (if (= (second most-freq) (second least-freq))
                     1
                     (first most-freq)))))

(defn search-co2
  [inputs]
  (search inputs (fn [[most-freq least-freq]]
                   (if (= (second most-freq) (second least-freq))
                     0
                     (first least-freq)))))

(defn part-2
  [inputs]
  (let [ox (search-ox inputs)
        co2 (search-co2 inputs)]
    (* (Integer/parseInt ox 2)
       (Integer/parseInt co2 2))))

(assert (= 230 (part-2 (map parse-input test-input))))
(part-2 (map parse-input final-input))
;; => 4481199
