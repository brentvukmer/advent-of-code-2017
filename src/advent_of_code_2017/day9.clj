(ns advent-of-code-2017.day9
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]))

;
;
; Full credit to vvvvalvalval for a working version of the EBNF I was trying for:
; https://github.com/vvvvalvalval/advent-of-code-2017/blob/master/src/aoc2017/day09.clj#L4-L12
;
;

;
; Part 1
;

(def day9-inputs
  (clojure.string/trim (slurp (io/resource "day9"))))

((defn create-parser
   ""
   []
   (insta/parser (clojure.java.io/resource "day9.bnf"))))

(def part1-parse-results
  ((create-parser) day9-inputs))

(defn score-groups
  ([groups-tree prev-score]
   (let [node-type (first groups-tree)
         group? (= :group node-type)
         children (if group? (rest groups-tree) '())
         leaf-node? (empty? children)
         increment (if group? 1 0)
         updated-score (+ prev-score increment)]
     (if
       leaf-node?
       (if group? updated-score 0)
       (apply + (cons updated-score (map #(score-groups % updated-score) (rest groups-tree))))
       )))
  ([groups-tree]
   (score-groups (second groups-tree) 0)))