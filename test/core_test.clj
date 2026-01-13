(ns core-test
  (:require [clojure.test :refer [deftest is]]
            [challenge.core :as sut]
            [challenge.readers])
  (:import (java.time LocalDate Period)
           (org.threeten.extra LocalDateRange)))

(def week0 #st/local-date-range "2024-01-01/P7D")
(def week1 #st/local-date-range "2024-01-08/P7D")
(def week2 #st/local-date-range "2024-01-15/P7D")
(def week3 #st/local-date-range "2024-01-22/P7D")
(def extra #st/local-date-range "2024-01-29/P3D")
(def month0 #st/local-date-range "2024-01-01/P1M")

(deftest ->range-ints
  (is (= (range 0 7)
         (sut/->range-ints (LocalDate/parse "2024-01-01")
                           week0)))
  (is (= (range 7 14)
         (sut/->range-ints (LocalDate/parse "2024-01-01")
                           week1)))
  (is (= (range 28 31)
         (sut/->range-ints (LocalDate/parse "2024-01-01")
                           extra))))

(deftest ->ldr
  (is (= (LocalDateRange/of (LocalDate/parse "2024-01-15") (Period/ofDays 7))
         (sut/->ldr (.getStart week2) (range 0 7))))
  (is (= (LocalDateRange/of (LocalDate/parse "2024-01-22") (Period/ofDays 7))
         (sut/->ldr (.getStart week3) (range 0 7))))
  (is (= (LocalDateRange/of (LocalDate/parse "2024-01-22") (Period/ofDays 7))
         (sut/->ldr (.getStart month0) (range 21 28)))))

(deftest connect-date-ranges
  (is (= #{month0} (sut/connect-date-ranges #{month0})))
  (is (= #{week0} (sut/connect-date-ranges #{week0})))
  (is (= #{month0} (sut/connect-date-ranges #{week0 week1 week2 week3 extra})))
  (is (= #{week0} (sut/connect-date-ranges #{#st/local-date-range "2024-01-01/P4D" #st/local-date-range "2024-01-03/P5D"})))
  (is (thrown? Exception (sut/connect-date-ranges #{week0 week2}))))

(deftest partition-intervals
  (is (= [[1] [4 5 6 7] [11 12]] (sut/partition-intervals '(1 4 5 6 7 11 12))))
  (is (= [[2 3 4 5]] (sut/partition-intervals '(2 3 4 5))))
  (is (= [[2 3] [5]] (sut/partition-intervals '(2 3 5))))
  (is (= [[2 3] [5] [7]] (sut/partition-intervals '(2 3 5 7))))
  (is (= [] (sut/partition-intervals '()))))
