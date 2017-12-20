(ns advent-of-code-2017.day2
 (:require [clojure.math.combinatorics :as combo]
           [clojure.java.io :as io]))

;
; TODO: Change this to use clojure.io/resource to load from classpath
;
(def day2-inputs (map #(map (fn [x] (Integer/parseInt x)) (clojure.string/split % #"\s")) (clojure.string/split-lines (slurp (io/resource "day2")))))

(def answer1 (reduce + (map #(let [sorted (sort %) min (first sorted) max (last sorted)] (- max min)) day2-inputs)))

(defn divide-by-lesser
  [x y]
  (if (> x y) 
     (/ x y)
     (/ y x)))

(defn get-division-result
  [row]
  (filter integer? (map #(divide-by-lesser (first %) (second %)) (combo/combinations row 2))))

(def answer2 (reduce + (mapcat get-division-result day2-inputs)))

