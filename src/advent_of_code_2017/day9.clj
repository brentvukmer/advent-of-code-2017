(ns advent-of-code-2017.day9
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(defn read-lines-classpath-resource
  ""
  [name]
  (clojure.string/split-lines (slurp (io/resource name))))

(def day9-inputs
  (map #(clojure.string/split % #"\s") (read-lines-classpath-resource "day9")))

(def day9-test-inputs
  (map #(clojure.string/split % #"\s") (clojure.string/split-lines "")))
