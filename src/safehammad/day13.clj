(ns safehammad.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (slurp (io/resource "day13-input-example.txt")))

(def input (slurp (io/resource "day13-input.txt")))

;; Part 1

(defn parse-dot [dot-str]
  (mapv read-string (str/split dot-str #",")))

(defn parse-fold [fold-str]
  (let [[_ axis n] (re-find #"([xy])=(\d+)" fold-str)]
    {:axis (keyword axis) :n (read-string n)}))

(defn parse-input [input]
  (let [[dot-str fold-str] (map str/split-lines (str/split input #"\n\n"))]
    [(map parse-dot dot-str) (map parse-fold fold-str)]))

(defn fold-dot [{:keys [axis n]} [x y]]
  (case axis
    :y (if (<= y n) [x y] [x (- (* 2 n) y)])
    :x (if (<= x n) [x y] [(- (* 2 n) x) y])))

(defn format-dots [dots]
  (let [max-x (apply max (map first dots))
        max-y (apply max (map second dots))
        dot-set (set dots)]
    (apply str
           (for [y (range (inc max-y))
                 x (range (inc max-x))]
             (str (when (zero? x) "\n") (if (dot-set [x y]) "#" "."))))))

(defn fold-dots [dots fold]
  (distinct (map (partial fold-dot fold) dots)))

(defn part-1 [input]
  (let [[dots folds] (parse-input input)
        result       (fold-dots dots (first folds))]
    ;(println (format-dots result))))
    ;[dots result]))
    (count result)))

(defn part-2 [input]
  (let [[dots folds] (parse-input input)
        result       (reduce (partial fold-dots) dots folds)]
    (println (format-dots result))))

(defn run
  [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
