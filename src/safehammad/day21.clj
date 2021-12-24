(ns safehammad.day21
  (:require [clojure.math.combinatorics :as combo]))

(def input-example {:p1 {:pos 4} :p2 {:pos 8}})

(def input {:p1 {:pos 4} :p2 {:pos 3}})

;; Ops

(defn move-player [amount pos]
  (-> pos dec (+ amount) (rem 10) inc))

(defn win? [winning-score {:keys [p1 p2]}]
  (some #(>= (:score %) winning-score) [p1 p2]))

;; Part 1

(def dice-rolls (map (partial apply +) (partition 3 (cycle (range 1 101)))))

(defn turn [{:keys [turn] :as state} dice]
  (let [new-pos (move-player dice (get-in state [turn :pos]))]
  (-> state
      (assoc-in [turn :pos] new-pos)
      (update-in [turn :score] (partial + new-pos))
      (update :dice-rolled (partial + 3))
      (update :turn #(if (= % :p1) :p2 :p1)))))

(defn result [{dice-rolled :dice-rolled, {p1-score :score} :p1, {p2-score :score} :p2}]
  (* dice-rolled (min p1-score p2-score)))

(defn part-1 [input]
  (let [state (merge-with merge input {:p1 {:score 0}
                                       :p2 {:score 0}
                                       :turn :p1
                                       :dice-rolled 0})]
    (result (first (drop-while (complement (partial win? 1000)) (reductions turn state dice-rolls))))))

;; Part 2

(defn player-wins [states player]
  (apply + (map :universes (filter #(>= (:score (player %)) 21) states))))

(defn win-count [states]
  {:p1 (player-wins states :p1)
   :p2 (player-wins states :p2)})

(defn remove-wins [states]
  (remove #(or
             (>= (:score (:p1 %)) 21)
             (>= (:score (:p2 %)) 21))
             states))

(def dice->universes
  "Possible rolls with their possible universes: {3 1, 4 3, 5 6, 6 7, 7 6, 8 3, 9 1}."
  (frequencies (map (partial apply +) (apply combo/cartesian-product (repeat 3 [1 2 3])))))

(defn turn-2 [{:keys [turn] :as state} [dice universes]]
  (let [new-pos (move-player dice (get-in state [turn :pos]))]
    (-> state
        (assoc-in [turn :pos] new-pos)
        (update-in [turn :score] (partial + new-pos))
        (update :universes (partial * universes))
        (update :turn #(if (= % :p1) :p2 :p1)))))

(defn apply-dice-rolls [state]
  (map (partial turn-2 state) dice->universes))

(defn merge-states [states]
  (map
    #(assoc (key %) :universes (apply + (map :universes (val %))))
    (group-by #(dissoc % :universes) states)))

(defn part-2 [input]
  (loop [states [(merge-with merge input {:p1 {:score 0}
                                          :p2 {:score 0}
                                          :turn :p1
                                          :universes 1N})]
         wins   {:p1 0N :p2 0N}]
    (if (empty? states)
      (apply max (vals wins))
      (let [new-states (merge-states (mapcat apply-dice-rolls states))]
        (recur
          (remove-wins new-states)
          (merge-with + wins (win-count new-states)))))))

(defn run [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
