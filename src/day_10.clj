(ns day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input
  ["[({(<(())[]>[[{[]{<()<>>"
   "[(()[<>])]({[<{<<[]>>("
   "{([(<{}[<>[]}>{[]{[(<()> "
   "(((({<>}<{<{<>}{[]{[]{}"
   "[[<[([]))<([[{}[[()]]]"
   "[{[{({}]{}}([{[{{{}}([]"
   "{<[[]]>}<{[{[{[]{()[[[]"
   "[<(<(<(<{}))><([]([]()"
   "<{([([[(<>()){}]>(<<{{"
   "<{([{{}}[<[[[<>{}]]]>[]]"])

(def final-input
  (-> "day-10.txt"
      io/resource
      slurp
      str/split-lines))

(def characters
  {\[ {:nature :opening :matching \]}
   \( {:nature :opening :matching \)}
   \< {:nature :opening :matching \>}
   \{ {:nature :opening :matching \}}
   \] {:nature :closing :matching \[}
   \) {:nature :closing :matching \(}
   \> {:nature :closing :matching \<}
   \} {:nature :closing :matching \{}})

(def part-1-points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def part-2-points
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn process-character
  [state character]
  (if (nil? character) (reduced state)
      (let [info     (get characters character)
            nature   (:nature info)
            matching (:matching info)
            to-close (last state)]
        (cond (and (nil? to-close) (= :opening nature))       [character]
              (and (= :closing nature) (= matching to-close)) (vec (butlast state))
              (= :opening nature)                             (conj state character)
              :else                                           (reduced [:corrupt character])))))

(defn part-1
  [input]
  (->> input
       (map (fn [line] (reduce process-character nil line)))
       (filter #(= (first %) :corrupt))
       (map second)
       frequencies
       (map (fn [[c n]] (* n (get part-1-points c))))
       (reduce +)))

(defn part-2
  [input]
  (let [scores (->> input
                    (map (fn [line] (reduce process-character nil line)))
                    (filter #(not= (first %) :corrupt))
                    (map reverse)
                    (map (fn [line] (map (comp :matching characters) line)))
                    (map (fn [line] (map part-2-points line)))
                    (map (fn [line] (reduce (fn [acc n] (+ n (* 5 acc))) line)))
                    sort)]
    (nth scores (quot (count scores) 2))))

(assert (= 26397 (part-1 demo-input)))
(part-1 final-input)
;; => 271245

(assert (= 288957 (part-2 demo-input)))
(part-2 final-input)
;; => 1685293086
