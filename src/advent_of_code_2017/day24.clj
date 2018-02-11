(ns advent-of-code-2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]))


;
; Part 1
;


(def input (->> "day24" io/resource io/reader line-seq))


(def sample-input (str/split-lines "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"))


(defn parse-components
  [input-str]
  (map #(edn/read-string (str "[" (str/replace % #"\/" " ") "]")) input-str))


(def components (parse-components input))


(def sample-components (parse-components sample-input))


(defn pairs
  [c components]

  (let [filtered (remove #(= c %) components)
        ;; Account for zero-ports; they can only match on second element
        s (if (zero? (first c))
            #{(second c)}
            (set c))]
    (filter (fn [x] (some s x)) filtered)))


(defn indexed
  [components]

  (into {}
        (map
          #(vector % (pairs % components))
          components)))


(defn bridges
  ""
  [indexed root]
  (let [seen (atom #{})]
    (tree-seq
      (fn [x] (when-not (@seen (last x))
                (swap! seen conj (last x))))
      #(map (fn [t] (conj % t))
            (filter (fn [y] (not (contains? @seen y)))
                    (get indexed (last %))))
      [root])))

(defn zero-ports
  [components]
  (let [sets (map set components)
        zeros (filter #(contains? % 0) sets)]
    (mapv vec zeros)))

(defn part1
  [components]
  (let [tree (indexed components)
        zeros (zero-ports components)]
    (last
      (sort
        (map #(reduce + (flatten %))
             (mapcat
               #(bridges
                  tree
                  %) zeros))))))


;
; Part 2
;

