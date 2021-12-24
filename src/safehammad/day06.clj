(ns safehammad.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input "3,4,3,1,2")

(def input (str/trim (slurp (io/resource "day06-input.txt"))))

(defn parse-input [input]
  (mapv #(Integer/parseInt %) (str/split input #",")))

(defn dec-day [day]
  (if (= day 0) 6 (dec day)))

;;; Part 1

(defn generate
  "Vector of days."
  [days]
  (let [new-fish (repeat (count (filter #{0} days)) 8)
        new-days (mapv dec-day days)]
    (concat new-days new-fish)))

(defn part-1 [input generations]
  (count (nth (iterate generate (parse-input input)) generations)))

;;; Part 2

(defn generate-2
  "Map of day frequencies."
  [days]
  (let [wrap-days (get days 0)]
    (-> days
        (update-keys #(mod (dec %) 9))  ; wrap from -1 to 8
        (update 6 + wrap-days))))

(defn part-2 [input generations]
  (->> (parse-input input)
       (frequencies)
       (merge (zipmap (range 9) (repeat 0)))
       (iterate generate-2)
       (#(nth % generations))
       (vals)
       (apply +)))

(defn run
  "Count number of questions"
  [part]
  (case part
    :part-1 (part-1 input 80)
    :part-2 (part-2 input 256)))
