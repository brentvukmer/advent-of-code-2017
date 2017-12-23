(ns advent-of-code-2017.day6
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

;
; Part 1
;

(defn read-lines-classpath-resource
  ""
  [name]
  (map #(Integer/parseInt %) (clojure.string/split (clojure.string/trim (slurp (io/resource name))) #"\t")))

(def day6-inputs
  (vec (read-lines-classpath-resource "day6")))

(defn indexed-vals
  ""
  [memory-banks]
  (group-by second (map vector (range) memory-banks)))

(defn max-val
  ""
  [indexed-vals]
  (apply max (keys indexed-vals)))

(defn first-max-index
  ""
  [indexed-vals max-val]
  (ffirst (get indexed-vals max-val)))

(defn find-most-blocks
  "Finds the memory bank with the most blocks.
  Ties won by the lowest-numbered memory bank."
  [memory-banks]
  (let [indexed-vals (indexed-vals memory-banks)
        max-val (max-val indexed-vals)]
    (first-max-index indexed-vals max-val)))

(defn next-index
  [current-index max-index]
  (if (< current-index max-index)
    (inc current-index)
    0))

(defn redistribute-blocks
  ""
  [memory-banks]
  (let [last-index (dec (count memory-banks))
        index-for-max (find-most-blocks memory-banks)
        max-value (get memory-banks index-for-max)]
    (loop [memory-banks (assoc-in memory-banks [index-for-max] 0)
           num-blocks max-value
           current-index (next-index index-for-max last-index)]
      (if (zero? num-blocks)
        memory-banks
        (recur (assoc-in memory-banks [current-index] (inc (get memory-banks current-index)))
               (dec num-blocks)
               (if (get memory-banks (inc current-index))
                 (inc current-index)
                 0))))))

(defn find-repeated-pattern
  ""
  [memory-bank]
  (loop [counter 0
         memory-bank memory-bank
         patterns #{}]
    (if (some #{memory-bank} patterns)
      counter
      (recur (inc counter)
             (redistribute-blocks memory-bank)
             (conj patterns memory-bank)))))