(ns safehammad.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/trim (slurp (io/resource "day16-input.txt"))))

(def hex->bin {\0 "0000"
               \1 "0001"
               \2 "0010"
               \3 "0011"
               \4 "0100"
               \5 "0101"
               \6 "0110"
               \7 "0111"
               \8 "1000"
               \9 "1001"
               \A "1010"
               \B "1011"
               \C "1100"
               \D "1101"
               \E "1110"
               \F "1111"})

(declare process-packet)

;; Parse

(defn parse-transmission [hex]
  (apply str (map hex->bin hex)))

(defn parse-bin [binary-string]
  (Long/parseLong binary-string 2))

(defn chomp-next [[pieces next-string] [parse-type length]]
  (let [[piece remaining] (mapv (partial apply str) (split-at length next-string))]
    [(conj pieces (cond-> piece (= parse-type :dec) parse-bin)) remaining]))

(defn chomp
  "Repeatedly peel off chars from string in length chunks.

  Specify length as :dec to parse binary to decimal, otherwise :bin.

  e.g. (chomp \"01110110010\" :dec 3 :bin 3)
  => 3 \"101\" \"10010\""
  [string & lengths]
  (flatten (reduce chomp-next [[] string] (partition 2 lengths))))

(defn chomp-header [packet]
  (chomp packet :dec 3 :dec 3))

;; Literal

(defn parse-literal-packet [fragment packet-header]
  (loop [parsed-literals []
         remaining       fragment
         bits-used       6]  ;; header
    (let [[start-bit number remaining]   (chomp remaining :dec 1 :bin 4)
          bits-used          (+ bits-used 5)
          parsed-literals    (conj parsed-literals number)]
      (case start-bit
        0 [(assoc packet-header :value (parse-bin (apply str parsed-literals))
                                :remaining remaining
                                :bits-used bits-used)]
        1 (recur parsed-literals remaining bits-used)))))

;; Operator

(defn create-operator-result [packet-header length-bit-count results]
  (let [sub-values (keep :value results)
        value (case (:packet-type-id packet-header)
                0 (apply + sub-values)
                1 (apply * sub-values)
                2 (apply min sub-values)
                3 (apply max sub-values)
                5 (if (apply > sub-values) 1 0)
                6 (if (apply < sub-values) 1 0)
                7 (if (apply = sub-values) 1 0))
        operator-packet (assoc packet-header :value value :bits-used (+ 6 1 length-bit-count))]  ; bits-used: header + length-type-id + 15/11
    [operator-packet results]))

(defn process-operator-packet [fragment packet-header]
  (let [[length-type-id remaining]    (chomp fragment :dec 1)
        length-bit-count              (case length-type-id
                                        0 15
                                        1 11)
        [sub-packet-length remaining] (chomp remaining :dec length-bit-count)]
    (loop [sub-packet-counter 0  ; bits used or num subpackets
           remaining remaining
           results []]
      (if (>= sub-packet-counter sub-packet-length)
        (create-operator-result packet-header length-bit-count results)
        (let [result (process-packet remaining)  ; fingers crossed stack doesn't blow
              flat-result (vec (flatten result))]
          (recur (+ sub-packet-counter (case length-type-id
                                         0 (apply + (map :bits-used flat-result))
                                         1 1))
                 (:remaining (peek flat-result))
                 (vec (concat results result))))))))

(defn process-packet [transmission]
  (let [[packet-version packet-type-id remaining] (chomp-header transmission)
        packet-header {:packet-version packet-version :packet-type-id packet-type-id}]
    (if (= 4 (:packet-type-id packet-header))
      (parse-literal-packet remaining packet-header)
      (process-operator-packet remaining packet-header))))

(comment
  ;; Test packets
  (process-packet "110100101111111000101000") ; literal
  (process-packet "00111000000000000110111101000101001010010001001000000000")   ; operator bits
  (process-packet "11101110000000001101010000001100100000100011000001100000"))  ; sub packet count

(defn part-1 [input]
  (apply + (map :packet-version (flatten (process-packet (parse-transmission input))))))

(defn part-2 [input]
  (:value (first (process-packet (parse-transmission input)))))

;(def input-example "620080001611562C8802118E34")
;(def input-example "C200B40A82")  ; sum = 3
;(def input-example "04005AC33890")  ; product = 54
;(def input-example "880086C3E88112")  ; min = 7
;(def input-example "CE00C43D881120")  ; max = 9
;(def input-example "D8005AC2A8F0")  ; < = 1
;(def input-example "F600BC2D8F")  ; > = 0
;(def input-example "9C005AC2F8F0")  ; != 0
;(def input-example "9C0141080250320F1802104A08")  ; != 0

(defn run [part]
  (case part
    :part-1 (part-1 input)
    :part-2 (part-2 input)))
