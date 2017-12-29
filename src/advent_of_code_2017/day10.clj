(ns advent-of-code-2017.day10
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(def day10-inputs
  (clojure.edn/read-string (str "[" (slurp (io/resource "day10")) "]")))

(defn get-shift-count
  "Number of items to shift to the front of the list of numbers"
  [curr-pos nums-count length]
  (if (= length nums-count)
    curr-pos
    (- nums-count length)))

(defn cycle-reverse-splice
  ""
  [nums curr-pos length]
  (if (= length 1)
    nums
    (let [nums-count (count nums)
          shift-count (get-shift-count curr-pos nums-count length)
          cycled (take nums-count (drop curr-pos (cycle nums)))
          reversed-section (vec (reverse (take length cycled)))
          spliced (if (= length nums-count)
                    (apply conj
                           (drop-last shift-count reversed-section)
                           (take-last shift-count reversed-section))
                    (apply conj
                           reversed-section
                           (take-last shift-count cycled)))]
      spliced)))

(defn circular-splice
  ""
  [spliced length]
  (vec
    (concat
      (take-last (dec length) spliced)
      (take (- (count spliced) (dec length)) spliced))))

(defn knot
  "'Tie a knot' in the list"
  [nums curr-pos length]
  ; TODO: Clean up this dirty code.
  (let [spliced (cycle-reverse-splice nums curr-pos length)
        length-index (+ curr-pos length)
        max-index (dec (count nums))]
    (if (and (> length-index max-index)
             (< length (count nums)))
      (circular-splice spliced length)
      spliced)))

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

(defn knots-hash
  ""
  [v]
  (* (first v) (second v)))


