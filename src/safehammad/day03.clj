(ns safehammad.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def example-input
  (str/split-lines
    "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"))

(def input
  "Series of entries from input file."
  (str/split-lines
    (slurp (io/resource "day03-input.txt"))))

(defn multiply-bins
  "Given one or more binary string, muliply together."
  [& bins]
  (apply * (map #(Integer/parseInt % 2) bins)))

(defn gamma [lines]
  (->> lines
       (apply map vector)
       (map frequencies)
       (map set/map-invert)
       (map #(get % (apply max (keys %))))
       (apply str)))

(defn invert
  "Flip bits of binary string."
  [gamma]
  (apply str (map #(if (= % \1) \0 \1) gamma)))

(defn part-1 [input]
  (let [gamma   (gamma input)
        epsilon (invert gamma)]
    (multiply-bins gamma epsilon)))

(defn rating-params [rating]
  (case rating
    :o2-rating  [max \1]
    :co2-rating [min \0]))

(defn favoured-bit [binaries index rating]
  (let [[choice-fn default-bit] (rating-params rating)
        freq                    (frequencies (map #(nth % index) binaries))]
    (if (apply = (vals freq))
      default-bit
      (get (set/map-invert freq) (apply choice-fn (vals freq))))))

(defn filter-binaries [binaries index favoured-bit]
  (filter #(= (nth % index) favoured-bit) binaries))

(defn calculate-number [binaries rating]
  (loop [lines binaries
         index 0]
    (if (= (count lines) 1)
      (first lines)
      (recur
        (filter-binaries lines index (favoured-bit lines index rating))
        (inc index)))))

(defn part-2 [binaries]
  (let [o2  (calculate-number binaries :o2-rating)
        co2 (calculate-number binaries :co2-rating)]
    (multiply-bins o2 co2)))

(defn run
  "Calculate diagnostics."
  [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
