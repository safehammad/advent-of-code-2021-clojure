(ns safehammad.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example (str/split-lines (slurp (io/resource "day10-input-example.txt"))))
(def input (str/split-lines (slurp (io/resource "day10-input.txt"))))

(def char-map {\( \)
               \[ \]
               \{ \}
               \< \>})

(def error-points {\) 3
                   \] 57
                   \} 1197
                   \> 25137})

(def incomplete-points {\) 1
                        \] 2
                        \} 3
                        \> 4})

;; Part 1

(defn open-char [acc next-char]
  (update acc :stack conj next-char))

(defn close-char [acc next-char]
  (let [top-char (-> acc :stack peek)]
    (if (= next-char (char-map top-char))
      (update acc :stack pop)
      (reduced (assoc acc :error next-char)))))

(defn process-char [acc next-char]
  (if ((set (keys char-map)) next-char)
    (open-char acc next-char)
    (close-char acc next-char)))

(defn error-char [line]
  (reduce process-char {:stack []} line))

(defn part-1 [input]
  (apply + (map error-points (keep :error (map error-char input)))))

(defn find-close [line]
  (map char-map line))

(defn assign-incomplete-points [line]
  (map incomplete-points line))

(defn total-points [numbers]
  (reduce #(+ (* 5 %1) %2) 0 numbers))

(defn middle-score [total-points-list]
  (let [n (/ (dec (count total-points-list)) 2)]
    (nth (sort total-points-list) n)))

(defn part-2 [input]
  (middle-score
    (map
    (comp total-points assign-incomplete-points find-close reverse :stack)
    (remove :error (map error-char input)))))

(defn run
  [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
