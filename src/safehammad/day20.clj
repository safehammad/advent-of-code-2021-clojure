(ns safehammad.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (slurp (io/resource "day20-input-example.txt")))

(def input (slurp (io/resource "day20-input.txt")))

;; Parse

(defn parse-image-input [image-input]
  (into {} (for [[row line] (map-indexed vector (str/split-lines image-input))
        [col pixel] (map-indexed vector line)]
    [[row col] pixel])))

(defn parse-input [input]
  (let [[enhancer image-input] (str/split input #"\n\n")]
    [enhancer (parse-image-input image-input)]))

;; Enhance

(defn pixel [image row col missing-pixel]
  (get image [row col] missing-pixel))

(defn pixel-binary-index [image row col missing-pixel]
  (Integer/parseInt
    (apply str
         (replace {\. "0" \# "1"}
                  (for [dr [-1 0 1]
                        dc [-1 0 1]]
                    (pixel image (+ row dr) (+ col dc) missing-pixel))))
    2))

(defn bounds [image]
  (let [coords       (keys image)
        rows  (map first coords)
        cols  (map last coords)]
    {:min-row (dec (apply min rows))
     :max-row (inc (apply max rows))
     :min-col (dec (apply min cols))
     :max-col (inc (apply max cols))}))

(defn enhanced-pixel [enhancer binary-index]
  (get enhancer binary-index))

(defn enhance-image [enhancer image missing-pixel]
  (let [{:keys [min-row max-row min-col max-col]} (bounds image)]
    (into {}
          (for [row (range (dec (dec min-row)) (inc max-row))
                col (range (dec (dec min-col)) (inc max-col))]
            [[row col] (enhanced-pixel enhancer (pixel-binary-index image row col missing-pixel))]))))

;; Run

(defn count-lit-pixels [image]
  (count (filter (partial = \#) (vals image))))

(defn calculate [enhancer image times]
  (count-lit-pixels (reduce
                      (partial enhance-image enhancer)
                      image
                      (take times (cycle [\. (get enhancer 0)])))))

(defn part-1 [input]
  (let [[enhancer image] (parse-input input)]
    (calculate enhancer image 2)))

(defn part-2 [input]
  (let [[enhancer image] (parse-input input)]
    (calculate enhancer image 50)))

(defn run [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
