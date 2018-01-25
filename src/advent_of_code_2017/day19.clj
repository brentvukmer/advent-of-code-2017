(ns advent-of-code-2017.day19
  (:require [clojure.java.io :as io]))

;
; Part 1
;
;
; Initial direction: down
; Initial location: first valid-path-char
; While there's a next valid-path-char in the current direction, proceed.
; If the next char is a valid-path-char but doesn't match the last char, skip ahead one.
; Otherwise, locate the neighbor that hasn't already been seen, change the direction to face that neighbor, and proceed.
; If the current char is alphabetic, add it to the list, then move to the "next" char.
;

(defn read-input
  [input-name]
  (clojure.string/split-lines (slurp (io/resource input-name))))
