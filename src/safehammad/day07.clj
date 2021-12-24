(ns safehammad.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input "16,1,2,0,4,2,7,1,2,14")

(def input (str/trim (slurp (io/resource "day07-input.txt"))))

(defn parse-input [input]
  (mapv #(Integer/parseInt %) (str/split input #",")))

(defn difference
  "Absolute difference."
  [a b]
  (Math/abs (- a b)))

(defn difference-triangle
  "Where n is difference between a and b: 1 + 2 + 3 + ... + n."
  [a b]
  (let [diff (difference a b)]
    (* (/ (inc diff) 2) diff)))

(defn total-fuel [fuel-fn positions alignment]
  (apply + (map (partial fuel-fn alignment) positions)))

(defn position-range
  "Inclusive range of positions."
  [positions]
  (range (apply min positions) (inc (apply max positions))))

(defn calculate [input fuel-fn]
  (let [positions (parse-input input)
        alignments (position-range positions)
        fuel-used (map (partial total-fuel fuel-fn positions) alignments)]
    ;; Look for minimum. Short circuit looking for when curve starts to rise.
    (->> (partition 2 1 fuel-used)
         (filter (fn [[a b]] (< a b)))
         ffirst)))

(defn run
  "Count bags."
  [part]
  (case part
    :part-1 (calculate input difference)
    :part-2 (calculate input difference-triangle)))
