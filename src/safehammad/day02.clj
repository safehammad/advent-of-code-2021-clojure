(ns safehammad.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input (str/split-lines "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"))

(def input (str/split-lines (slurp (io/resource "day02-input.txt"))))

(defn parse-line [line]
  (let [[cmd n] (str/split line #" ")]
    [cmd (Integer/parseInt n)]))

(defn update-pos [[horizontal depth] [cmd n]]
  (case cmd
    "forward" [(+ horizontal n) depth]
    "down"    [horizontal (+ depth n)]
    "up"      [horizontal (- depth n)]))

(defn update-pos-2 [[aim horizontal depth] [cmd n]]
  (case cmd
    "forward" [aim (+ horizontal n) (+ depth (* aim n))]
    "down"    [(+ aim n) horizontal depth]
    "up"      [(- aim n) horizontal depth]))

(defn run [part]
  (case part
    :part-1 (apply * (reduce update-pos [0 0] (map parse-line input)))  ; [horiz depth]
    :part-2 (apply * (rest (reduce update-pos-2 [0 0 0] (map parse-line input))))))  ; [aim horiz depth]
