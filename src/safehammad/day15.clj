(ns safehammad.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (slurp (io/resource "day15-input-example.txt")))

(def input (slurp (io/resource "day15-input.txt")))

;; Parse

(defn parse-line [row line]
  (map-indexed (fn [col risk] [[row col] (read-string (str risk))]) line))

(defn parse-input [input]
  (into {} (apply concat (map-indexed parse-line (str/split-lines input)))))

;; Ops

(defn max-x-y [risk-map]
  (let [coords (keys risk-map)
        max-x  (apply max (map first coords))
        max-y  (apply max (map second coords))]
    [max-x max-y]))

(defn manhatten-fn [risk-map]
  (let [[max-x max-y] (max-x-y risk-map)]
    (fn [[row col]]
      (+ (- max-x col) (- max-y row)))))

(defn neighbour-coords-fn [risk-map]
  (let [[max-x max-y] (max-x-y risk-map)]
    (fn [[row col]]
      (for [[dr dc] [[-1 0] [0 -1] [1 0] [0 1]]
            :let [new-row (+ row dr)
                  new-col (+ col dc)]
            :when (and (<= 0 new-row max-y) (<= 0 new-col max-x))]
        [new-row new-col]))))

(defn add-node [risk-map h-fn path coord]
  (let [g (+ (:g path) (get risk-map coord))
        h (h-fn coord)]
    (assoc path :f (+ g h) :g g :h h :coord coord)))

(defn extend-path-fn
  "Return neighbouring paths."
  [risk-map h-fn]
  (let [make-neighbour-coords (neighbour-coords-fn risk-map)]
    (fn [path] (map (partial add-node risk-map h-fn path) (make-neighbour-coords (:coord path))))))

(defn next-path
  "Find next path with lowest f to extend."
  [open-paths]
  (apply min-key :f (vals open-paths)))

(defn improved-path?
  "Return true if better paths already exist."
  [open-paths closed-paths {:keys [coord f]}]
  (let [open-f   (:f (get open-paths coord))
        closed-f (:f (get closed-paths coord))]
    (and
      (if open-f (< f open-f) true)
      (if closed-f (< f closed-f) true))))

(defn best-path [risk-map]
  (let [h-fn        (manhatten-fn risk-map)
        extend-path (extend-path-fn risk-map h-fn)
        h0          (h-fn [0 0])]
    (loop [open-paths   {[0 0] {:f h0 :g 0 :h h0 :coord [0 0]}}
           closed-paths {}]
      (let [path             (next-path open-paths)
            successor-paths  (filter (partial improved-path? open-paths closed-paths) (extend-path path))
            new-open-paths   (into (dissoc open-paths (:coord path)) (map #(vector (:coord %) %) successor-paths))
            new-closed-paths (assoc closed-paths (:coord path) path)
            complete-path    (first (filter (comp zero? :h) successor-paths))]
        (if complete-path
          complete-path
          (recur new-open-paths new-closed-paths))))))

;; Part 1

(defn part-1 [input]
  (let [risk-map (parse-input input)]
    (:g (best-path risk-map))))

;; Part 2

(defn adjust-risk [risk amount]
  (inc (mod (dec (+ risk amount)) 9)))

(defn five-times-risk [width height [row col] risk]
  (for [dr (range 5)
        dc (range 5)]
    [[(+ row (* height dr)) (+ col (* width dc))] (adjust-risk risk (+ dr dc))]))

(defn five-squared-map [max-x max-y risk-map]
  (mapcat (fn [[coord risk]] (five-times-risk max-x max-y coord risk)) risk-map))

(defn part-2 [input]
  (let [risk-map        (parse-input input)
        [max-x max-y]   (max-x-y risk-map)
        large-risk-map  (into {} (five-squared-map (inc max-x) (inc max-y) risk-map))]
    (:g (best-path large-risk-map))))

(defn run
  [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
