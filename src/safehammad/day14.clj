(ns safehammad.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (slurp (io/resource "day14-input-example.txt")))

(def input (slurp (io/resource "day14-input.txt")))

;; Parse

(defn parse-insertion [insertion-str]
  (str/split insertion-str #" -> "))

(defn parse-input [input]
  (let [[template insertion-str] (str/split input #"\n\n")]
    [template (into {} (map parse-insertion (str/split-lines insertion-str)))]))

(defn template->pairs [template]
  (map
    (partial apply str)
    (partition 2 1 template)))

;; Part 1

(defn next-template [insertions template]
  (let [pairs      (template->pairs template)
        insertions (map (partial get insertions) pairs)]
    (apply str
           (conj (vec (interleave template insertions)) (last template)))))

(defn nth-template [insertion-map template n]
  (nth (iterate (partial next-template insertion-map) template) n))

(defn calculate [element-freqs]
  (let [freqs (vals element-freqs)]
    (- (apply max freqs) (apply min freqs))))

(defn part-1 [input]
  (let [[template insertions] (parse-input input)
        elements              (nth-template insertions template 10)]
    (calculate (frequencies elements))))

;; Part 2

(defn expand-pair [insertion-map [pair n]]
  (let [insertion (get insertion-map pair)
        [a b]     pair]
  [{(str a insertion) n} {(str insertion b) n}]))

(defn next-pair-freq [insertion-map pair-freq]
  (apply merge-with + (mapcat (partial expand-pair insertion-map) pair-freq)))

(defn nth-pair-freq [insertion-map pair-freq n]
  (nth (iterate (partial next-pair-freq insertion-map) pair-freq) n))

(defn part-2 [input]
  (let [[template insertion-map] (parse-input input)
        pair-freq                (frequencies (template->pairs template))
        last-letter              (str (last template))]
    (->> (nth-pair-freq insertion-map pair-freq 40)
         (mapv #(hash-map (subs (key %) 0 1) (val %)))
         (apply merge-with + {last-letter 1})
         calculate)))

(defn run
  [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
