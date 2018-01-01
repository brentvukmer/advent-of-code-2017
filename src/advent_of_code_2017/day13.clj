(ns advent-of-code-2017.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;
; Part 1
;

(def day13-input-str
  (slurp (io/resource "day13")))

(defn tokens->map
  ""
  [[key-str val-str]]
  (let [key (Integer/parseInt (string/trim key-str))
        val (Integer/parseInt (clojure.string/trim val-str) )]
    {:depth key :range val}))

(defn lines->tokens
  ""
  [input-str]
  (map tokens->map
       (map #(string/split % #":")
            (string/split-lines input-str))))

(def day13-layers
  (vec (lines->tokens day13-input-str)))

