(ns advent-of-code-2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))


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


(defn find-open-port
  [prev]
  (let [freqs (frequencies (remove zero? (flatten prev)))
        available-port (first (filter #(odd? (get freqs %)) (keys freqs)))]
    available-port))


(defn uses-open-port
  [prev potential-next]
  (let [open-port (find-open-port prev)]
    (contains? (set potential-next) open-port)))


;
; This code is gross.  Find a cleaner solution.
;
(defn bridges
  ""
  [indexed root]

  (tree-seq
    #(some? (seq %))
    #(map (fn [t] (conj % t))
          (filter (fn [y] (and
                            (not (contains? (set %) y))
                            (uses-open-port % y)))
                  (get indexed (last %))))
    [root]))


(defn zero-ports
  [components]
  (let [zeros (filter #(contains? (set %) 0) components)]
    (mapv vec zeros)))


(defn part1
  [components]
  (let [tree (indexed components)
        zeros (zero-ports components)]
    (last
      (sort
        (map #(reduce + (flatten %))
             (mapcat #(bridges tree %) zeros))))))


;
; Part 2
;


(defn part2
  [components]
  (let [tree (indexed components)
        zeros (zero-ports components)
        bridges (mapcat #(bridges tree %) zeros)
        grouped-by-length (group-by count bridges)
        max-length (apply max (keys grouped-by-length))]
    (apply max
      (map #(reduce + (flatten %))
           (get grouped-by-length max-length)))))
