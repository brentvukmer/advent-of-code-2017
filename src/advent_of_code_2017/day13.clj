(ns advent-of-code-2017.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;
; Part 1
;

(def day13-input-str
  (slurp (io/resource "day13")))

(defn line-tokens->map
  ""
  [[key-str val-str]]
  (let [key (Integer/parseInt (string/trim key-str))
        val (Integer/parseInt (clojure.string/trim val-str) )]
    {:depth key :range val}))

(defn lines->tokens
  ""
  [input-str]
  (map #(string/split % #":")
       (string/split-lines input-str)))

(defn input->layers
  ""
  [input-str]
  (let [tokens (lines->tokens input-str)]
    (vec (map line-tokens->map tokens))))

(def day13-layers
  (input->layers day13-input-str))

(defn scanner-pos
  ""
  [t layer]
  (let [range (:range layer)
        pos (mod t range)]
    (assoc layer :scanner-pos pos)))

(defn scanner-positions
  ""
  [t layers]
  (map #(scanner-pos t %) layers))



