(ns safehammad.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.core.logic :as l :refer [run* distincto membero everyg lvar ==]]
            [clojure.set :as set]))

(def example-input (str/split-lines (slurp (io/resource "day08-input-example.txt"))))

(def input (str/split-lines (slurp (io/resource "day08-input.txt"))))

(def sample-row "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(def digits {'[a b c e f g]   '0
             '[c f]           '1
             '[a c d e g]     '2
             '[a c d f g]     '3
             '[b c d f]       '4
             '[a b d f g]     '5
             '[a b d e f g]   '6
             '[a c f]         '7
             '[a b c d e f g] '8
             '[a b c d f g]   '9})

;;; Part 1

(combo/permutations [:a :b :c])

(defn parse-line-1 [line]
  (->> line
       (map count)
       (filter #{2 3 4 7})))  ; segments in 1 4 7 8

(defn parse-input-1 [input]
  (->> input
       (map #(str/split % #" \| "))
       (map second)
       (map #(str/split % #" "))
       (map parse-line-1)))

(defn part-1 [input]
  (count (apply concat (parse-input-1 input))))

;;; Part 2

(defn possible-segments [segment-count]
  (filter #(= (count %) segment-count) (keys digits)))

(defn decode [segments]
  (let [letters  (sort (distinct (map first segments)))
        lvars    (repeatedly (count letters) lvar)
        lvar-map (zipmap letters lvars)
        result   (vals (select-keys lvar-map letters))]
    (map (partial zipmap letters)
         (run* [q]
               (everyg (fn [[l coll]] (membero (get lvar-map l) coll)) segments)
               (distincto result)
               (== q result)))))

(defn expand-segments [[encoded decoded]]
  (map #(vector % decoded) encoded))

(defn parse-row [row side]
  (let [side-fn (case side
                  :left first
                  :right second)]
    (-> row
      (str/split #" \| ")
      side-fn
      (str/split #" "))))

(defn parse-signal-patterns [row side]
  (map (partial mapv (comp symbol str)) (parse-row row side)))

(defn ->signal-map [signal-patterns possible-segments]
  (mapcat (partial apply map vector) (map vector (combo/permutations signal-patterns) (repeat possible-segments))))

(defn make-signal-patterns [row]
  (let [count->signal-patterns (group-by count (parse-signal-patterns row :left))]
    (mapcat
      (fn [[n patterns]] (->signal-map patterns (possible-segments n)))
      (filter (fn [[n patterns]] (#{2 3 4} n)) count->signal-patterns))))

(defn missing-letters
  "Letters not in a-g from given letters."
  [letters]
  (set/difference '#{a b c d e f g} (set letters)))

;; I couldn't get the logic programming only solution to be performant, so here's the cheat
(defn expand-mapping [mapping]
  (let [hard-keys (missing-letters (keys mapping))
        hard-vals (missing-letters (vals mapping))]
    (map (partial into mapping) (map #(map vector % hard-vals) (combo/permutations hard-keys)))))

(defn possible-mappings [row]
  (let [easy-mappings (decode (mapcat expand-segments (make-signal-patterns row)))]
    (mapcat expand-mapping easy-mappings)))

(defn validate-mapping [signal-patterns mapping]
  (let [digit-sets (set (map set (keys digits)))
        valid?     (every? digit-sets (map (comp set (partial replace mapping)) signal-patterns))]
    (when valid? mapping)))

(defn get-mapping [row]
  (some (partial validate-mapping (parse-signal-patterns row :left)) (possible-mappings row)))

(defn output-value [row]
  (let [number-map (into {} (map (juxt (comp set key) val) digits))
        output-patterns (map (comp set (partial replace (get-mapping row))) (parse-signal-patterns row :right))]
    (read-string (str/replace (apply str (replace number-map output-patterns)) #"^0*" ""))))

(defn part-2 [input]
  (apply + (map output-value input)))

(defn run [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
