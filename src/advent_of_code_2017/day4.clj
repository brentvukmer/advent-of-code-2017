(ns advent-of-code-2017.day4
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(def day4-inputs (map #(clojure.string/split % #" ") (clojure.string/split-lines (clojure.string/trim (slurp (io/resource "day4"))))))

(def answer-part1 (count (filter #(= (count %) (count (set %))) day4-inputs)))
