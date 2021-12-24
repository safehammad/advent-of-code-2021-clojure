(ns safehammad.aoc2021-test
  (:require [clojure.test :refer [deftest is testing]]
            [safehammad.day01 :as day01]
            [safehammad.day02 :as day02]
            [safehammad.day03 :as day03]
            [safehammad.day04 :as day04]
            [safehammad.day05 :as day05]
            [safehammad.day06 :as day06]
            [safehammad.day07 :as day07]
            [safehammad.day08 :as day08]
            [safehammad.day09 :as day09]
            [safehammad.day10 :as day10]
            [safehammad.day11 :as day11]
            [safehammad.day12 :as day12]
            [safehammad.day13 :as day13]
            [safehammad.day14 :as day14]
            [safehammad.day15 :as day15]
            [safehammad.day16 :as day16]
            [safehammad.day17 :as day17]
            [safehammad.day18 :as day18]
            [safehammad.day20 :as day20]
            [safehammad.day21 :as day21]
            [safehammad.day22 :as day22]))

(deftest day01-test
  (testing "Day 1, part 1."
    (is (= 1342 (day01/run :part-1))))
  (testing "Day 1, part 2."
    (is (= 1378 (day01/run :part-2)))))

(deftest day02-test
  (testing "Day 2, part 1."
    (is (= 1990000 (day02/run :part-1))))
  (testing "Day 2, part 2."
    (is (= 1975421260 (day02/run :part-2)))))

(deftest day03-test
  (testing "Day 3, part 1."
    (is (= 3985686 (day03/run :part-1))))
  (testing "Day 3, part 2."
    (is (= 2555739 (day03/run :part-2)))))

(deftest day04-test
  (testing "Day 4, part 1."
    (is (= 34506 (day04/run :part-1))))
  (testing "Day 4, part 2."
    (is (= 7686 (day04/run :part-2)))))

(deftest day05-test
  (testing "Day 5, part 1."
    (is (= 7318 (day05/run :part-1))))
  (testing "Day 5, part 2."
    (is (= 19939 (day05/run :part-2)))))

(deftest day06-test
  (testing "Day 6, part 1."
    (is (= 386640 (day06/run :part-1))))
  (testing "Day 6, part 2."
    (is (= 1733403626279 (day06/run :part-2)))))

(deftest day07-test
  (testing "Day 7, part 1."
    (is (= 353800 (day07/run :part-1))))
  (testing "Day 7, part 2."
    (is (= 98119739 (day07/run :part-2)))))

(deftest day08-test
  (testing "Day 8, part 1."
    (is (= 294 (day08/run :part-1))))
  (testing "Day 8, part 2."
    (is (= 973292 (day08/run :part-2)))))

(deftest day09-test
  (testing "Day 9, part 1."
    (is (= 535 (day09/run :part-1))))
  (testing "Day 9, part 2."
    (is (= 1122700 (day09/run :part-2)))))

(deftest day10-test
  (testing "Day 10, part 1."
    (is (= 319233 (day10/run :part-1))))
  (testing "Day 10, part 2."
    (is (= 1118976874 (day10/run :part-2)))))

(deftest day11-test
  (testing "Day 11, part 1."
    (is (= 1713 (day11/run :part-1))))
  (testing "Day 10, part 2."
    (is (= 502 (day11/run :part-2)))))

(deftest day12-test
  (testing "Day 12, part 1."
    (is (= 3463 (day12/run :part-1))))
  (testing "Day 12, part 2."
    (is (= 91533 (day12/run :part-2)))))

(deftest day13-test
  (testing "Day 13, part 1."
    (is (= 770 (day13/run :part-1))))
  (testing "Day 13, part 2."
    (is (= nil (day13/run :part-2)))))  ; View output: EPUELPBR

(deftest day14-test
  (testing "Day 14, part 1."
    (is (= 3831 (day14/run :part-1))))
  (testing "Day 14, part 2."
    (is (= 5725739914282 (day14/run :part-2)))))

(deftest day15-test
  (testing "Day 15, part 1."
    (is (= 553 (day15/run :part-1))))
  (testing "Day 15, part 2."
    (is (= 2858 (day15/run :part-2)))))

(deftest day16-test
  (testing "Day 16, part 1."
    (is (= 860 (day16/run :part-1))))
  (testing "Day 16, part 2."
    (is (= 470949537659 (day16/run :part-2)))))

(deftest day17-test
  (testing "Day 17, part 1."
    (is (= 4095 (day17/run :part-1))))
  (testing "Day 17, part 2."
    (is (= 3773 (day17/run :part-2)))))

(deftest day18-test
  (testing "Day 18, part 1."
    (is (= 4176 (day18/run :part-1))))
  (testing "Day 18, part 2."
    (is (= 4633 (day18/run :part-2)))))

(deftest day20-test
  (testing "Day 20, part 1."
    (is (= 5339 (day20/run :part-1))))
  (testing "Day 20, part 2."
    (is (= 18395 (day20/run :part-2)))))

(deftest day21-test
  (testing "Day 21, part 1."
    (is (= 734820 (day21/run :part-1))))
  (testing "Day 21, part 2."
    (is (= 193170338541590N (day21/run :part-2)))))

(deftest day22-test
  (testing "Day 22, part 1."
    (is (= 650099 (day22/run :part-1))))
  (testing "Day 22, part 2."
    (is (= 1254011191104293 (day22/run :part-2)))))
