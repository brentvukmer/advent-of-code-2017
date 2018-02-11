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

  (filter (fn [x] (or (= (second c) (first x))
                      (= (second c) (second x))))
          (remove #(= c %) components)))


(defn indexed
  [components]

  (into {}
        (map
          #(vector % (pairs % components))
          components)))


(defn bridges
  ""
  [indexed root]
  (tree-seq (let [seen (atom #{})]
              (fn [x] (when-not (@seen (last x))
                        (swap! seen conj (last x)))))
            #(map (fn [t] (conj % t)) (get indexed (last %)))
            [root]))


(defn part1
  [root components]
  (last
    (sort
      (map #(reduce + (flatten %))
           (bridges
             (indexed components)
             root)))))


;
; Part 2
;

