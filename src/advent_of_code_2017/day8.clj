(ns advent-of-code-2017.day8
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(defn read-lines-classpath-resource
  ""
  [name]
  (clojure.string/split-lines (slurp (io/resource name))))

(def day8-inputs
  (map #(clojure.string/split % #"\s") (read-lines-classpath-resource "day8")))

(def day8-test-inputs
  (map #(clojure.string/split % #"\s") (clojure.string/split-lines "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10")))

(defn initial-registers
  ""
  [inputs]
  (into {} (map vector (set (map (comp keyword first) inputs)) (repeat 0))))