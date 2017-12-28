(ns advent-of-code-2017.day10
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(def day10-inputs
  (clojure.edn/read-string (str "[" (slurp (io/resource "day10")) "]")))

(defn circ-subvec
  "Subvec for a circular list"
  [v index count]
  (vec (take count (drop index (cycle v)))))

(defn knot-hash
  ""
  ([lengths]
   (knot-hash (range 0 256) lengths))
  ([nums lengths]
   (knot-hash nums 0 0 lengths))
  ([nums curr-pos skip-size lengths]
    ;; nums - list of numbers (by default, from 0 to 255)
    ;; curr-pos - current position which begins at 0 (the first element in the list)
    ;; skip-size - a skip size (which starts at 0)
    ;; lengths - a sequence of lengths (your puzzle input)
   0))



