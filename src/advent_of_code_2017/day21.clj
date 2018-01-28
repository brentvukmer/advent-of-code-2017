(ns advent-of-code-2017.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def start-pattern-input ".#.\n..#\n###")

(def start-pattern (mapv vec (str/split-lines start-pattern-input)))


;
; Part 1
;


(defn parse-rule-input
  ""
  [rule-input]
  (let [parsed
        (mapv
          #(mapv vec (str/split % #"\/"))
          (str/split rule-input #"\s=>\s"))
        from (first parsed)
        to (second parsed)]
    {:input-size (count from)
     :from       from
     :to         to}))


(defn parse-rules-from
  ""
  [input-name]
  (mapv parse-rule-input
        (str/split-lines
          (slurp
            (io/resource input-name)))))


;
; Rotate clockwise by 90 degrees
;
(defn rotate
  ""
  [pixels]
  (apply mapv #(vec (reverse %&)) pixels))


;
; flip up
; flip right to left
;
(defn flip
  ""
  [pixels direction]
  (cond
    (= direction :up)
    (map #(vec (reverse %)) pixels)
    :else
    []))


; Break the pixels up into 2x2 squares
; For each 2x2 square:
; - Find the corresponding enhancement rule (flip or rotate as needed)
; - Use rule to convert 2x2 square into a 3x3 square
; Merge 3x3 squares into one
(defn two->three
  ""
  [pixels]
  [2 3])


; Break the pixels up into 3x3 squares.
; For each 3x3 square:
; - Find the corresponding enhancement rule (flip or rotate as needed)
; - Use rule to convert 3x3 square into a 4x4 square
; Merge 4x4 squares into one
(defn three->four
  ""
  [pixels]
  [3 4])


(defn split-and-convert
  ""
  [pixels]
  (let [size (count (first pixels))]
    (cond
      (= 0 (mod size 2))
      (two->three pixels)
      (= 0 (mod size 3))
      (three->four pixels)
      :else
      :error)))

;
; Part 2
;

