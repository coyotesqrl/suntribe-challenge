(ns challenge.core
  (:import (java.time LocalDate Period)
           (java.time.temporal ChronoUnit)
           (org.threeten.extra LocalDateRange)))

(defn ->range-ints
  "Transforms the provided interval into a range of ints representing the
  delta (in days) from the start date."
  [start interval]
  (let [st-delta (.until start (.getStart interval) ChronoUnit/DAYS)
        days (.lengthInDays interval)]
    (range st-delta (+ st-delta days))))

(defn ->ldr
  "Parses a seq of ints into a LocalDateRange.
  The start date of the resulting LDR is given as start plus the first value in the seq (the seq is assumed
  to be in natural order). The Period is the count of ints in the seq."
  [start ints]
  (let [offset-start (.plusDays start (first ints))]
    (LocalDateRange/of ^LocalDate offset-start ^Period (Period/ofDays (count ints)))))

(defn connect-date-ranges
  "Takes a set of date ranges - with no gaps - and converts it to a set containing
  the union of all those ranges. If any of the ranges in the input are unconnected,
  an exception will be thrown.

  Example: converts the set of intervals #{2024-01-15/P7D 2024-01-01/P7D 2024-01-08/P7D}
  to a set with a single range: #{2024-01-01/P21D}"
  [intervals]
  (let [[f & r] (sort-by #(.getStart %) intervals)]
    (conj #{} (reduce (fn [a v]
                        (.union a v))
                      f
                      r))))

(defn partition-intervals
  "Given the sorted input, partition on gaps.
  Example: for the input (1 4 5 6 7 11 12), returns the vector [[1] [4 5 6 7] [11 12]]."
  [input]
  (let [{:keys [curr acc]} (reduce (fn [{:keys [curr] :as a} v]
                                     (if (empty? curr)
                                       (update a :curr conj v)
                                       (if (= v (inc (last curr)))
                                         (update a :curr conj v)
                                         (-> a (update :acc conj curr) (assoc :curr (vector v))))))
                                   {:curr [] :acc []}
                                   input)]
    (if (seq curr)
      (conj acc curr)
      acc)))

(defn process-diff
  [report-period intervals]
  (let [report-start (.getStart report-period)
        interval-ints (->> intervals
                           (apply into)
                           (map (partial ->range-ints report-start))
                           (apply concat))
        report-ints (->range-ints report-start report-period)]

    (->> report-ints
         (remove (set interval-ints))
         (partition-intervals)
         (map (partial ->ldr report-start))
         (set))))

(defn difference [& input]
  (condp = (count input)
    0 #{}
    1 (connect-date-ranges (first input))
    (process-diff (first (connect-date-ranges (first input))) (rest input))))
