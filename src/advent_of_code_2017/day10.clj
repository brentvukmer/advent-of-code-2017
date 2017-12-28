(ns advent-of-code-2017.day10
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(def day10-inputs
  (clojure.edn/read-string (str "[" (slurp (io/resource "day10")) "]")))

(defn knot
  "'Tie a knot' in the list"
  [nums curr-pos length]
  ; TODO: Clean up this dirty code.
  (let [nums-count (count nums)
        remaining-count (- nums-count length)
        cycled (take nums-count (drop curr-pos (cycle nums)))
        reversed-section (vec (reverse (take length cycled)))
        spliced (apply conj reversed-section (take-last remaining-count cycled))
        knotted (concat (take-last (dec remaining-count) spliced) (take (dec length) spliced))]
    knotted))

(defn do-knots
  "Take a given list of numbers (0-255 by default) and list of lengths, and 'tie a knot' in the list."
  ([lengths]
   (do-knots (range 0 256) lengths))
  ([nums lengths]
   (do-knots nums 0 0 lengths))
  ([nums curr-pos skip-size lengths]
    ;; nums - list of numbers (by default, from 0 to 255)
    ;; curr-pos - current position which begins at 0 (the first element in the list)
    ;; skip-size - a skip size (which starts at 0)
    ;; lengths - a sequence of lengths (your puzzle input)
   (loop [nums nums
          curr-pos curr-pos
          skip-size skip-size
          lengths lengths]
     (if (empty? lengths)
       nums
       (recur (knot nums curr-pos (first lengths))
              (mod (+ curr-pos (first lengths) skip-size) (count nums))
              (inc skip-size)
              (rest lengths))))))



