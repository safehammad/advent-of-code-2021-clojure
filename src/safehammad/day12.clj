(ns safehammad.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-example-1
  (map str/trim
       (str/split-lines "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")))

(def input-example-2
  (map str/trim
       (str/split-lines "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc")))

(def input-example-3
  (map str/trim
       (str/split-lines "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")))

(def input (str/split-lines (slurp (io/resource "day12-input.txt"))))

;; Ops

(defn small-cave? [cave]
  (Character/isLowerCase (first cave)))

(defn path-complete? [path]
  (= "end" (:tip path)))

(defn next-caves [connections cave]
  (get connections cave))

(defn extend-path [path next-cave]
  (cond-> path
    (small-cave? next-cave) (update-in [:small-visits next-cave] (fnil inc 0))
    true                    (assoc :tip next-cave)
    true                    (update :caves conj next-cave)))

(defn path-extensions [connections path invalid-path-fn]
  (remove invalid-path-fn (map (partial extend-path path) (next-caves connections (:tip path)))))

(defn input->connections [input]
  (apply merge-with into
         (map (partial apply hash-map)
              (map (juxt first (comp vector second))
                   (mapcat (juxt identity reverse)
                           (map #(str/split % #"-") input))))))

(defn valid-paths [connections invalid-path-fn]
  (loop [complete-paths #{}
         paths          [{:tip "start" :small-visits {"start" 1} :caves ["start"]}]]
    (if (empty? paths)
      complete-paths
      (let [[next-path remaining-paths] ((juxt peek pop) paths)
            extended                     (path-extensions connections next-path invalid-path-fn)
            [complete incomplete]        ((juxt filter remove) path-complete? extended)]
        (recur
          (into complete-paths complete)
          (into remaining-paths incomplete))))))

;; Part 1

(defn invalid-path-part-1
  "Return true if a small cave is revisited."
  [path]
  (some (partial < 1) (vals (:small-visits path))))

;; Part 2

(defn invalid-path-part-2
  "Return true if more that one small cave, or start, is visited twice or more."
  [path]
  (let [freq (frequencies (vals (get path :small-visits)))]
    (or
      (> (get-in path [:small-visits "start"]) 1)
      (> (get freq 2 0) 1)
      (get freq 3))))

;; Main fn

(defn calculate
  [input invalid-path-fn]
  (count (valid-paths (input->connections input) invalid-path-fn)))

(comment
  (path-extensions (input->connections input-example-1) {:tip "start" :small-visits {"start" 1} :caves ["start"]} invalid-path-part-1))

(comment
  (invalid-path-part-2 {:tip "start" :small-visits {"start" 1 "b" 3} :caves ["start"]}))

(defn run
  [part]
  (case part
    :part-1 (calculate input invalid-path-part-1)
    :part-2 (calculate input invalid-path-part-2)))
