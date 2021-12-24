(ns safehammad.day17)

(def input-example "target area: x=20..30, y=-10..-5")

(def input "target area: x=244..303, y=-91..-54")

(defn parse-input [input]
  (zipmap
    [:min-x :max-x :min-y :max-y]
    (map read-string (rest (re-find #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" input)))))

(defn drag-x-vel
  "Drag x velocity toward 0."
  [x-vel]
  (+ x-vel (cond
             (pos? x-vel)  -1
             (neg? x-vel)  1
             (zero? x-vel) 0)))

(defn move-probe [{[x y] :pos [x-vel y-vel] :velocity}]
  {:pos [(+ x x-vel) (+ y y-vel)]
   :velocity [(drag-x-vel x-vel) (dec y-vel)]})

(defn hit-or-miss [{:keys [min-x max-x min-y max-y]} {[x y] :pos}]
  (cond
    (< y min-y)                                 :miss
    (and (<= min-x x max-x) (<= min-y y max-y)) :hit
    :else                                       false))

(defn launch-probe
  "Return result of launch."
  [target-area velocity]
  (let [[initial-steps terminal-steps] (split-with
                                         (complement (partial hit-or-miss target-area))
                                         (iterate move-probe {:pos [0 0] :velocity velocity}))
        terminal-step  (first terminal-steps)
        termination    (hit-or-miss target-area terminal-step)]

    (case termination
      :hit  {:outcome :hit :max-y (apply max (map #(-> % :pos second) initial-steps))}
      :miss {:outcome :miss
             :overshoot? (> (-> terminal-step :pos first) (:max-x target-area))})))

(defn trial-y-velocity [target-area part y-velocity ]
  (let [before-overshoot
        (cond->> (map #(launch-probe target-area [% y-velocity]) (range 0 (inc (:max-x target-area))))
          (= part :part-1) (take-while #(not (:overshoot? %))))
        hits (filter #(= (:outcome %) :hit) before-overshoot)]
    (cond
      (seq hits) (assoc (last hits) :hit-count (count hits) :y-velocity y-velocity)
      :else      {:outcome :miss :y-velocity y-velocity})))

;; Not proud of the (take 200), but eyeballing and brute force worked!

(defn part-1 [input]
  (let [target-area (parse-input input)]
    (:max-y (last (filter
                    #(= :hit (:outcome %))
                    (take 200 (map (partial trial-y-velocity target-area :part-1) (range))))))))

(defn part-2 [input]
  (let [target-area (parse-input input)]
    (apply + (map :hit-count (filter
                               #(= :hit (:outcome %))
                               (take 200 (map (partial trial-y-velocity target-area :part-2) (range (:min-y target-area) Integer/MAX_VALUE))))))))

(defn run [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
