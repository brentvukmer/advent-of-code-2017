(ns advent-of-code-2017.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

;
; Part 1
;

(def day12-input-str
  (slurp (io/resource "day12")))

(defn tokens->map-entry
  ""
  [[key-str val-str]]
  (let [key (Integer/parseInt (string/trim key-str))
        val (clojure.edn/read-string (str "[" (clojure.string/trim val-str) "]"))]
    [key val]))

(defn lines->tokens
  ""
  [input-str]
  (map tokens->map-entry
       (map #(string/split % #"<->")
            (string/split-lines input-str))))

(def day12-map
  (into {} (lines->tokens day12-input-str)))

(defn connected
  ""
  [program-id pipes-map]
  (loop [visited #{ program-id }
         directs (set (get pipes-map program-id))]
    (let [combined (set/union visited directs)]
      (if (= visited combined)
        visited
        (recur (set/union visited directs)
               (set (mapcat #(get pipes-map %) directs)))))))




