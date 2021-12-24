(ns safehammad.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.math.combinatorics :as combo]))

(def input (str/split-lines (slurp (io/resource "day18-input.txt"))))

;; Parse

(defn matched-square-brackets? [string]
  (let [freqs (frequencies string)]
    (= (freqs \[) (freqs \]))))

(defn split-node-content [content]
  (let [splits (str/split content #",")]
    (->> (map #(split-at % splits) (range 1 (count splits)))
         (map (partial map (partial str/join ",")))
         (filter (partial every? matched-square-brackets?))
         first)))

(defn parse-snailfish [node]
  (if (re-matches #"\d+" node)
    (read-string node)
    (let [content (last (re-find #"\[(.*)\]" node))]
      (mapv parse-snailfish (split-node-content content)))))

;; Ops

(defn pair? [v]
  (and
    (vector? v)
    (= (count v) 2)
    (every? int? v)))

;; Explode

(defn depth [loc]
 (count (zip/path loc)))

(defn explode-add-right [loc right-value]
  (loop [loc loc
         explode-reached? false]
    (let [node (zip/node loc)]
      (cond
        (= :exploded node)                 (recur (zip/next (zip/replace loc 0)) true)
        (zip/end? loc)                     loc
        (and explode-reached? (int? node)) (zip/edit loc (partial + right-value))
        :else                              (recur (zip/next loc) explode-reached?)))))

(defn explode-add-left [loc left-value]
  (loop [loc loc]
    (let [node (zip/node loc)]
      (if (int? node)
        (zip/edit loc (partial + left-value))
        (recur (zip/prev loc))))))

(defn explode-number [number]
  (loop [loc          (zip/vector-zip number)
         pair         nil
         previous-int? false]
    (if (or pair (zip/end? loc))
      {:pair pair
       :previous-int? previous-int?
       :loc loc}
      (let [next-loc        (zip/next loc)
            next-node       (zip/node next-loc)
            previous-int?   (if (int? next-node) true previous-int?)
            [new-next pair] (if (and
                                  (pair? next-node)
                                  (>= (depth next-loc) 4))
                              [(zip/replace next-loc :exploded) next-node]
                              [next-loc nil])]
        (recur
          new-next
          pair
          previous-int?)))))

(defn explode [root]
  (zip/root
    (let [{:keys [pair previous-int? loc]} (explode-number root)]
      (cond-> loc
        (and pair previous-int?) (explode-add-left (first pair))
        pair                     (explode-add-right (last pair))))))

;; Split

(defn split-node [value]
  [(quot value 2) (quot (inc value) 2)])

(defn split [root]
  (zip/root
    (loop [loc (zip/vector-zip root)]
      (let [node (zip/node loc)]
        (cond
          (and (int? node) (>= node 10)) (zip/edit loc split-node)
          (zip/end? loc)   loc
          :else            (recur (zip/next loc)))))))

;; Add

(defn add
  "Add two snailfish numbers."
  [left right]
  [left right])

;; Reduce

(defn reduce-snailfish-number [number]
    (let [exploded (explode number)]
      (if (not= exploded number)
        (reduce-snailfish-number exploded)
        (let [splitted (split exploded)]
          (if (not= splitted exploded)
            (reduce-snailfish-number splitted)
            splitted)))))

(defn add-number-list [number-list]
  (reduce
    (fn [left right]
      (reduce-snailfish-number (add left right)))
    number-list))

;; Magnitude

(defn magnitude [[left right]]
  (let [left-value (if (int? left) left (magnitude left))
        right-value (if (int? right) right (magnitude right))]
    (+
     (* 3 left-value)
     (* 2 right-value))))

;; Run!
;; N.B. With hindsight, this would have been much better tackled by
;; keeping the number as text, but I got to learn about Clojure zippers!

(defn calculate [input]
  (magnitude (add-number-list (map parse-snailfish input))))

(defn part-1 [input]
  (calculate input))

(defn part-2 [input]
  (apply max (map calculate (combo/permuted-combinations input 2))))

;(def input-example "[[[[[9,8],1],2],3],4]")
;(def input-example "[[6,[5,[4,[3,2]]]],1]")
;(def input-example "[[6,[5,[4,1]]],1]")
;(def input-example "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
;(def input-example "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
;(def input-example ["[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]"])
;(def input-example (map str/trim (str/split-lines "[1,1]
;                                                  [2,2]
;                                                  [3,3]
;                                                  [4,4]
;                                                  [5,5]
;                                                  [6,6]")))
;(def input-example (str/split-lines "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
;[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
;[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
;[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
;[7,[5,[[3,8],[1,4]]]]
;[[2,[2,2]],[8,[8,1]]]
;[2,9]
;[1,[[[9,3],9],[[9,0],[0,7]]]]
;[[[5,[7,4]],7],1]
;[[[[4,2],2],6],[8,7]]"))

;(def input-example (str/split-lines "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
;[[[5,[2,8]],4],[5,[[9,9],0]]]
;[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
;[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
;[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
;[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
;[[[[5,4],[7,7]],8],[[8,3],8]]
;[[9,3],[[9,9],[6,[4,9]]]]
;[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
;[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"))

(defn run [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
