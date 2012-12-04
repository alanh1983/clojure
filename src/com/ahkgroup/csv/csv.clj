(ns com.ahkgroup.csv
  (:use clojure.test)
  (:require [clojure.string :as str]))

(defn- split-on-new-lines [string]
  (str/split string #"\r?\n|\r|\n"))

(defn- split-on-comma [string]
  (str/split string #","))

(defn- lines-contain-same-no-of-fields? [lines]
  (let [distinct-field-counts (distinct (map #(count (split-on-comma %)) lines))]
    (= (count distinct-field-counts) 1)))

(defn- a-line-ends-with-comma? [lines]
  (some true? (map #(.endsWith % ",") lines)))

(defn- check-parsable [lines]
  (if (a-line-ends-with-comma? lines)
    (throw (IllegalArgumentException. "A line ends with a comma"))
    (if (not (lines-contain-same-no-of-fields? lines))
      (throw (IllegalArgumentException. "Lines must contain equal no of cells")))))

;Change function to use threading macro
;Move production code out to a new namespace and file
(defn parse [text]
  (if (= "" text) ""
    (let [lines (split-on-new-lines text)]
      (check-parsable lines)
        (map #(map str/trim %)
          (map #(split-on-comma (% 0))
            (map vector lines))))))

(deftest empty-string
  (is (= "" (parse ""))))

(deftest single-line-comma-separated-split
  (let [result (parse "1,2")]
    (is (= [["1", "2"]] result))))

(deftest windows-new-line-processed-correctly
  (let [result (parse "1,2\r\n3,4")]
    (is (= [["1", "2"]["3", "4"]] result))))

(deftest mac-new-line-processed-correctly
  (let [result (parse "1,2\r3,4")]
    (is (= [["1", "2"]["3", "4"]] result))))

(deftest unix-new-line-processed-correctly
  (let [result (parse "1,2\n3,4")]
    (is (= [["1", "2"]["3", "4"]] result))))

(deftest last-field-should-not-end-with-comma
  (is (thrown-with-msg? IllegalArgumentException #"A line ends with a comma" (parse "1,2,")))
  (is (thrown? IllegalArgumentException #"A line ends with a comma" (parse "1,2\n3,4,")))
  (is (thrown? IllegalArgumentException #"A line ends with a comma" (parse "1,2,\n3,4"))))

(deftest all-lines-should-be-same-length
  (is (thrown-with-msg? IllegalArgumentException #"Lines must contain equal no of cells" (parse "1,2\n3"))))

(deftest splits-correctly-with-whitespace-around-comma
  (let [result-space-after (parse "A, B")
        result-space-before (parse "A ,B")
        result-space-before-and-after (parse "A , B")]
    (is (= [["A", "B"]] result-space-after))
    (is (= [["A", "B"]] result-space-before))
    (is (= [["A", "B"]] result-space-before-and-after))))

(deftest comma-in-cell-text-requires-quoting
  (let [
        non-quoted-cell1 "A,1"
        quoted-cell1 "\"A,1\""
        cell2 "B"
        non-quoted-result (parse (str/join [non-quoted-cell1, ",", cell2]))
        quoted-result (parse (str/join [quoted-cell1, ",", cell2]))]
    (is (= [["A", "1", "B"]] non-quoted-result))
;    (is (= [["A,1", "B"]] quoted-result))
    ))

(run-tests 'com.ahkgroup.csv)

;(println (some true? (map #(.endsWith % ",") ["1,2" "3,4"])))
