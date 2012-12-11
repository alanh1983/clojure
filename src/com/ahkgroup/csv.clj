(ns com.ahkgroup.csv
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