(ns advent-of-code-2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]))


;
; Part 1
;


(def input (->> "day24" io/resource io/reader line-seq))


(def components (sort (map #(edn/read-string (str "[" (str/replace % #"\/" " ") "]")) input)))


(def start-components (into #{} (filter #(zero? (first %)) components)))


(def remaining-components (filter #(not (contains? start-components %)) components))


(def combos (combo/combinations components 2))


(def valid-combos (filter #(= (second (first %)) (first (second %))) combos))


;
; Part 2
;

