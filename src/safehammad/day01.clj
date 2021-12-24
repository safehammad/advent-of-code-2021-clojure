(ns safehammad.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input [199 200 208 210 200 207 240 269 260 263])

(def input
  "Seq of numbers from input file."
  (map #(Integer/parseInt %)
     (str/split-lines
       (slurp (io/resource "day01-input.txt")))))

(defn window-input
  "Group numbers by 3 then sum."
  [numbers]
  (map (partial apply +) (partition 3 1 numbers)))

(defn count-increases
  "Count instances of number larger than previous."
  [numbers]
  (count (filter (fn [[a b]] (< a b)) (partition 2 1 numbers))))

(defn run
  "Count number of times numbers increase."
  [part]
  (case part
    :part-1 (count-increases input)
    :part-2 (count-increases (window-input input))))
