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
  ""
  [input-name]
  (clojure.string/split-lines (slurp (io/resource input-name))))

(defn non-space-indexes
  ""
  [inputs]
  (for [i (range (count inputs))
        :let [line (get inputs i)]]
    (filter some? (map-indexed (fn [index item] (if (not (Character/isSpace item)) [[i index] item] nil)) line))))

(defn parse-diagram
  ""
  [inputs]
  (->> (non-space-indexes inputs)
       (apply concat)
       (into {})))

(defn get-next
  ""
  [index remaining-indexes direction]
  {:next-index [0 0] :next-direction [0 0]})

(defn follow-path
  ""
  [path-info]
  (let [indexes (sort (keys path-info))]
    (loop [index (first indexes)
           direction [1 0]
           letters []
           remaining-indexes (rest indexes)]
      (let [c (get path-info index)
            updated-letters (if (Character/isAlphabetic c) (conj letters c) letters)
            [next-index next-direction] (get-next index remaining-indexes direction) ]
        (if (= 0 (second index))
          updated-letters
          (recur next-index
                 next-direction
                 updated-letters
                 (remove #(= index %) remaining-indexes)))))))