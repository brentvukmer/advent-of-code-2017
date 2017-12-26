(ns advent-of-code-2017.day9
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]))

;
; Part 1
;

(defn read-lines-classpath-resource
  ""
  [name]
  (slurp (io/resource name)))

(def day9-inputs
  (read-lines-classpath-resource "day9"))

(def day9-parser (insta/parser (clojure.java.io/resource "day9.bnf")))



