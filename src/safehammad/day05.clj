(ns safehammad.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input
  (map str/trim
       (str/split-lines "0,9 -> 5,9
                        8,0 -> 0,8
                        9,4 -> 3,4
                        2,2 -> 2,1
                        7,0 -> 7,4
                        6,4 -> 2,0
                        0,9 -> 2,9
                        3,4 -> 1,4
                        0,0 -> 8,8
                        5,5 -> 8,2")))

(def input
  "Series of lines from input file."
  (str/split-lines
    (slurp (io/resource "day05-input.txt"))))

(defn vertical? [[x1 _ x2 _]]
  (= x1 x2))

(defn horizontal? [[_ y1 _ y2]]
  (= y1 y2))

(defn inclusive-range
  "Inclusive range agnostic to whether increasing or decreasing."
  [a b]
  (if (<= a b) (range a (inc b)) (range a (dec b) -1)))

(defn points [line]
  (let [[x1 y1 x2 y2] line]
    (cond
      (vertical? line)   (map #(vector x1 %) (inclusive-range y1 y2))
      (horizontal? line) (map #(vector % y1) (inclusive-range x1 x2))
      :else              (map vector (inclusive-range x1 x2) (inclusive-range y1 y2)))))  ; diagonal

(range 3 4)

(defn parse-lines [input]
  (->> input
       (map #(str/split % #" -> "))
       (map (partial map #(str/split % #",")))
       (map (partial apply concat))
       (map (partial map #(Integer/parseInt %)))))

(defn calculate [input diagonals?]
  (let [line-filter-fn (if diagonals? some? (some-fn horizontal? vertical?))]
    (->> (parse-lines input)
         (filter line-filter-fn)
         (map points)
         (apply concat)
         (frequencies)
         (vals)
         (filter (partial <= 2))
         (count))))

(defn run
  [part]
  (case part
    :part-1 (calculate input false)
    :part-2 (calculate input true)))
