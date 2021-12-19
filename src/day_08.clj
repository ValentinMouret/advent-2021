(ns day-08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(defn parse-line
  [line]
  (let [[head tail] (str/split line #" | ")]
    [(str/split head #" ")
     (str/split tail #" ")]))

(defn parse-input
  [input]
  (-> input
      (str/split #"\R")
      (->> (map #(str/split % #" \| " ))
           (map (fn [[head tail]]
                  [(str/split head #" ")
                   (str/split tail #" ")])))))

(def demo-input
  (parse-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

(def final-input
  (-> "day-08.txt"
      io/resource
      slurp
      parse-input))

(defn identify-in-line
  [[_ output]]
  (let [by-n-digits (group-by count output)]
    (->> by-n-digits
         (filter #(some #{2 4 3 7} #{(first %)}))
         (map second)
         flatten
         count)))

(assert (= 26 (->> demo-input
                   (map identify-in-line)
                   (reduce +))))


(->> final-input
     (map identify-in-line)
     (reduce +))
