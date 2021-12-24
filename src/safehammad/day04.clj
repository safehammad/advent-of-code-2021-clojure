(ns safehammad.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (str/split (slurp (io/resource "day04-input.txt")) #"\n\n"))

(defn parse-numbers [input]
  (map #(Integer/parseInt %) (str/split (first input) #",")))

(defn parse-board-row [row]
  (map #(Integer/parseInt %) (str/split (str/trim row) #"\s+")))

(defn parse-board [line]
  (map parse-board-row (str/split-lines line)))

(defn parse-boards [input]
  (map parse-board (rest input)))

(defn row-sets [board]
  (map set board))

(defn col-sets [board]
  (apply map hash-set board))

(defn win? [drawn-numbers board]
  (let [number-set (set drawn-numbers)
        wins       (filter #(set/subset? % number-set) (concat (row-sets board) (col-sets board)))]
    (seq wins)))

(defn winning-boards [{:keys [boards drawn-numbers]}]
  (filter (partial win? drawn-numbers) boards))

(defn score [{:keys [drawn-numbers winning-board]}]
  (let [board-numbers (flatten winning-board)]
    (* (peek drawn-numbers) (apply + (set/difference (set board-numbers) (set drawn-numbers))))))

(defn board-count [state]
  (count (:boards state)))

(defn draw-number [win-choice state number]
  (let [new-state (as-> state state
                    (update state :drawn-numbers conj number)
                    (update state :winners (comp vec distinct into) (winning-boards state)))
        winners   (:winners new-state)
        win-board (case win-choice
                    :first-win (first winners)
                    :last-win (when (= (count winners) (board-count new-state))
                                (last winners)))]
    (if win-board (reduced (assoc new-state :winning-board win-board)) new-state)))

(defn run-part [win-choice input]
  (let [numbers (parse-numbers input)
        boards  (parse-boards input)
        state   {:drawn-numbers [], :boards boards, :winners []}]
    (score (reduce (partial draw-number win-choice) state numbers))))

(defn run [part]
  (case part
    :part-1 (run-part :first-win input)
    :part-2 (run-part :last-win input)))
