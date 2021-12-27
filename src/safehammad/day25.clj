(ns safehammad.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (map str/trim (str/split-lines "v...>>.vv>
                                                  .vv>>.vv..
                                                  >>.>v>...v
                                                  >>v>>.>.v.
                                                  v>v.vv.v..
                                                  >.>>..v...
                                                  .vv..>.>v.
                                                  v.v..>>v.v
                                                  ....v..v.>")))

(def input (str/split-lines (slurp (io/resource "day25-input.txt"))))

;; Parse

(defn parse-input [input]
  (into {}
        (for [[row line] (map-indexed vector input)
              [col item] (map-indexed vector line)]
          [[row col] item])))

;; Display

(defn format-fish-map [rows cols coords]
  (for [row (range rows)]
    (println (apply str (for [col (range cols)]
                          (get coords [row col]))))))

;; Part 1

(defn wrap [rows cols [row col]]
   [(mod row rows) (mod col cols)])

(defn add-coords [coords-1 coords-2]
  (mapv + coords-1 coords-2))

(defn free? [fish-map coord]
  (= (fish-map coord) \.))

(defn move-item [rows cols fish-map motion [coord item]]
  (let [next-coord (wrap rows cols (add-coords coord motion))]
    (when (free? fish-map next-coord)
      [[coord \.] [next-coord item]])))

(defn move [rows cols fish-map chosen-item motion]
  (into fish-map
        (mapcat
          (partial move-item rows cols fish-map motion)
          (filter
            (fn [[_ item]] (= item chosen-item))
            fish-map))))

(defn step [row-count col-count fish-map]
  (reduce (partial apply move row-count col-count) fish-map [[\> [0 1]] [\v [1 0]]]))

(defn part-1 [input]
  (let [fish-map              (parse-input input)
        [row-count col-count] ((juxt count (comp count first)) input)]
    (inc (count (take-while
                  #(apply not= %)
                  (partition 2 1 (iterate (partial step row-count col-count) fish-map)))))))

(defn run [part]
  (case part
    :part-1 (part-1 input)))
