(ns safehammad.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (map str/trim (str/split-lines "on x=10..12,y=10..12,z=10..12
                                                  on x=11..13,y=11..13,z=11..13
                                                  off x=9..11,y=9..11,z=9..11
                                                  on x=10..10,y=10..10,z=10..10")))

(def input-example-part-1 (str/split-lines (slurp (io/resource "day22-input-example-part-1.txt"))))

(def input-example-part-2 (str/split-lines (slurp (io/resource "day22-input-example-part-2.txt"))))

(def input (str/split-lines (slurp (io/resource "day22-input.txt"))))

;; Parse

(defn parse-step [step-string]
  (let [items (rest (re-find #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" step-string))]
    (-> (zipmap [:x :y :z] (partition 2 (map read-string (next items))))
        (assoc :on-off (if (= (first items) "on") 1 -1)))))

(defn parse-input [input]
  (map parse-step input))

;; Part 1

(defn light-cubes [cubes {on-off :on-off [min-x max-x] :x [min-y max-y] :y [min-z max-z] :z}]
  (into cubes (for [x (range min-x (inc max-x))
                    y (range min-y (inc max-y))
                    z (range min-z (inc max-z))]
                [[x y z] on-off])))

(defn count-lit-cubes [cubes]
  (count (filter (partial = 1) (vals cubes))))

(defn clamp [step k]
  (update step k #(let [[min-val max-val] %] [(max min-val -50) (min max-val 50)])))

(defn adjust-step
  "Clamp to -50..50."
  [step]
  (reduce #(clamp %1 %2) step [:x :y :z]))

(defn part-1 [input]
  (count-lit-cubes (reduce light-cubes {} (map adjust-step (parse-input input)))))

;; Part 2

(defn cuboid-count [{:keys [on-off x y z]}]
  (* on-off (apply * (map (comp inc (partial apply -) reverse) [x y z]))))

(defn intersect-range [[min-a max-a] [min-b max-b]]
  (when-not (or (< max-b min-a) (< max-a min-b))
    [(max min-a min-b) (min max-a max-b)]))

(defn intersection [{x-next :x y-next :y z-next :z} {:keys [on-off x y z]}]
  (let [intersect-ranges (map intersect-range [x-next y-next z-next] [x y z])]
    (when (every? some? intersect-ranges)
      (assoc (zipmap [:x :y :z] intersect-ranges) :on-off (- on-off)))))  ; invert on/off of intersection

(defn add-cuboid [cuboids cuboid]
  (cond-> (into cuboids (keep (partial intersection cuboid) cuboids))
    (pos? (:on-off cuboid)) (conj cuboid)))

(defn part-2 [input]
  (apply + (map cuboid-count (reduce add-cuboid [] (parse-input input)))))

(defn run [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
