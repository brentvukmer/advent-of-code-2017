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

