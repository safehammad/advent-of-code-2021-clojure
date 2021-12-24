(ns safehammad.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (str/split-lines (slurp (io/resource "day11-input-example.txt"))))
(def input (str/split-lines (slurp (io/resource "day11-input.txt"))))

(defn neighbour-self-coords [[row col]]
  (for [dr [-1 0 1]
        dc [-1 0 1]
        :let [new-coord [(+ row dr) (+ col dc)]]
        :when (and
                (every? #(<= 0 % 9) new-coord)
                (not (= dr dc 0)))]
    new-coord))

(defn will-flash? [{:keys [octo-map]}]
  (some (partial <= 10) (vals octo-map)))

(defn flash-octopus
  "Increase octopus and those around it by 1."
  [coord]
  (map #(hash-map % 1) (neighbour-self-coords coord)))

(defn flash-octo-map
  "Update octo-map based on flashes i.e. 10."
  [{:keys [flashes octo-map]}]
  (let [flash-coords (keys (filter #(-> % val (>= 10)) octo-map))]
    {:flashes (+ flashes (count flash-coords))
     :octo-map (into
                 (apply merge-with + octo-map (mapcat (partial flash-octopus) flash-coords))
                 (map #(vector % -10000000) flash-coords))}))

(defn inc-octo-map [octo-map]
  (into {} (map (juxt key (comp inc val)) octo-map)))

(defn set-zeroes [octo-map]
  (into {} (map (juxt key (comp #(if (neg? %) 0 %) val)) octo-map)))

(defn step [state]
  (update
    (first (drop-while will-flash? (iterate flash-octo-map (update state :octo-map inc-octo-map))))
    :octo-map
    set-zeroes))

(defn print-octo-map
  "Output function to check we're on the right track."
  [octo-map]
  (println
    (apply str (for [row (range 10)
                     col (range 10)]
                 (let [n (format "%3d" (get octo-map [row col]))]
                   (str n (when (= col 9) "\n")))))))

(defn parse-input
  "Convert input to map of coords to numbers."
  [input]
  (into {}
        (for [[r row] (map-indexed vector input)
              [c value] (map-indexed vector row)]
          [[r c] (read-string (str value))])))

(defn steps [octo-map]
  (iterate step {:flashes 0, :octo-map octo-map}))

(defn part-1 [input]
  (let [octo-map (parse-input input)]
    (:flashes (nth (steps octo-map) 100))))

;;; Part 2

(defn all-same? [{:keys [octo-map]}]
  (apply = (vals octo-map)))

(defn part-2 [input]
  (let [octo-map (parse-input input)]
    (count (take-while (complement all-same?) (steps octo-map)))))

(defn run
  [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
