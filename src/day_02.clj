(ns day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Keywords are unnecessary here, but they look good.
;; [:forward X]: increases horizontal position by X
;; [:down X]: Increases depth by X.
;; [:up X]: Decreases depth by X.
(def test-input
  [[:forward 5]
   [:down 5]
   [:forward 8]
   [:up 3]
   [:down 8]
   [:forward 2]])

(defn parse-line
  [line]
  (let [[movement X] (str/split line #" ")]
    [(keyword movement)
     (Integer/parseInt X)]))

(def final-input
  (->> "day-02.txt"
       io/resource
       slurp
       str/split-lines
       (map parse-line)))

(defn change-state-1
  [[horizontal depth] [dir X]]
  (cond (= dir :up) [horizontal (- depth X)]
        (= dir :down) [horizontal (+ depth X)]
        (= dir :forward) [(+ horizontal X) depth]))

(defn position
  [movements]
  (reduce change-state-1 [0 0] movements))

(defn part-1
  [movements]
  (reduce * (position movements)))

(assert (= 150 (part-1 test-input)))
(part-1 final-input)
;; => 1698735

(defn change-state-2
  [[aim [horizontal depth]] [dir X]]
  (cond (= :up dir) [(- aim X) [horizontal depth]]
        (= :down dir) [(+ aim X) [horizontal depth]]
        (= :forward dir) [aim [(+ X horizontal) (+ depth (* X aim))]]))

(defn part-2
  [movements]
  (->> movements
       (reduce change-state-2 [0 [0 0]])
       second
       (reduce *)))

(assert (= 900 (part-2 test-input)))
(part-2 final-input)
;; => 1594785890
