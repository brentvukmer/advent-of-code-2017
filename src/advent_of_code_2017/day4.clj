(ns advent-of-code-2017.day4
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(def day4-inputs (map #(clojure.string/split % #" ") (clojure.string/split-lines (clojure.string/trim (slurp (io/resource "day4"))))))

(defn no-duplicate-words
  "docstring"
  [passphrase]
  (= (count passphrase) (count (set passphrase))))

(def answer-part1 (count (filter #(no-duplicate-words %) day4-inputs)))

;
; Part 2
;

(defn no-anagrams
  ""
  [passphrase]
  (= (count passphrase) (count (set (map set passphrase)))))

(def answer-part2 (count (filter #(no-anagrams %) (filter #(no-duplicate-words %) day4-inputs))))
