(ns advent-of-code-2017.day1
  (:require [clojure.java.io :as io]))

(def nums (map #(Integer/valueOf (str %)) (seq (clojure.string/trim (slurp (io/resource "day1"))))))

(def answer1 (reduce + (map #(if (= %1 %2) %1 0) nums (conj (vec (rest nums)) (first nums)))))

(def rotated-nums (apply concat ((juxt drop take) (/ (count nums) 2) nums)))

(def answer2 (reduce + (map #(if (= (first %) (second %)) (first %) 0) (map list nums rotated-nums))))
