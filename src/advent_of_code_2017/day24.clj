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


(def combos (combo/combinations components 2))


(defn pairs
  [c components]

  (filter (fn [x] (= (second c) (first x)))
          (remove #(= c %) components)))


(defn usable-components
  [first components]

  (tree-seq
    #(not (empty? (pairs % components)))
    #(pairs % components)
    first))


(defn bridges
  [first components]

  (let [usable (usable-components first components)]
    (loop [paths []
           node (first usable)
           remaining (rest usable)]
      (if (empty? remaining)
        paths
        (let [children (pairs node remaining)]
          )))))


;
; Part 2
;

