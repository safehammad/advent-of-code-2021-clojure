(ns safehammad.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def example-input (str/split-lines (slurp (io/resource "day09-input-example.txt"))))

(def input (str/split-lines (slurp (io/resource "day09-input.txt"))))

(defn value-at [height-map row col]
  (-> height-map
      (get row)
      (get col)))

(defn neighbour-coords [row col]
  (map (fn [[dr dc]] [(+ row dr) (+ col dc)])[[-1 0] [0 -1] [1 0] [0 1]]))

(defn neighbours [height-map row col]
  (keep
    (fn [[row col]]
      (let [value (value-at height-map row col)]
        (when value {:value value, :coords [row col]})))
    (neighbour-coords row col)))

(defn parse-row [row]
  (mapv #(Integer/parseInt (str %)) row))

(defn find-minima [height-map]
  (filter some?
          (for [row (range (count height-map))
                col (range (count (first height-map)))]
            (let [value (value-at height-map row col)]
              (when (every? (partial < value) (map :value (neighbours height-map row col)))
                {:value value, :coords [row col]})))))

(defn part-1 [input]
  (let [height-map (mapv (partial parse-row) input)
        minima     (find-minima height-map)]
    (apply + (count minima) (map :value minima))))

(defn valid-basin-coord? [visited base-coord {:keys [value] :as coord}]
  (and
    (not (visited coord))
    (not= value 9)
    (< (:value base-coord) value)))

(defn basin-coords [height-map minimum]
  (loop [visited                #{}
         to-visit               #{minimum}]
    (let [next-coord       (first to-visit)
          neighbour-coords (apply neighbours height-map (:coords next-coord))
          expansion        (filter (partial valid-basin-coord? visited next-coord) neighbour-coords)
          new-to-visit     (into (disj to-visit next-coord) expansion)
          new-visited      (conj visited next-coord)]
      (if (empty? new-to-visit)
        new-visited
        (recur new-visited new-to-visit)))))

(defn part-2 [input]
  (let [height-map (mapv (partial parse-row) input)
        minima     (find-minima height-map)]
    (apply * (take 3 (sort > (map (comp count (partial basin-coords height-map)) minima))))))

(defn run
  [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
