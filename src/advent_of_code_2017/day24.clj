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


(defn bridges
  [first components]
  (loop [node first
         tree-paths '([[first]])
         remaining (remove #(= first %) components)]
    (if (empty? remaining)
      tree-paths
      (let [updated-paths (map
                            #(map
                               (fn [x] (conj % x))
                               (pairs (last %) components)) tree-paths)
            tbd (flatten up)]))))


;
; Part 2
;

