(ns advent-of-code-2017.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

;
; Part 1
;

(def day13-input-str
  (slurp (io/resource "day13")))

(defn line-tokens->map-entry
  ""
  [[key-str val-str]]
  (let [depth (Integer/parseInt (string/trim key-str))
        range (Integer/parseInt (clojure.string/trim val-str))]
    [depth range]))

(defn input->line-tokens
  ""
  [input-str]
  (map #(string/split % #":")
       (string/split-lines input-str)))

(defn input->layers
  ""
  [input-str]
  (let [tokens (input->line-tokens input-str)
        layers-from-input (into {} (map line-tokens->map-entry tokens))
        input-depths (map first layers-from-input)
        all-depths (range
                     0
                     (inc (last input-depths)))
        missing-depths (set/difference
                         (set all-depths)
                         (set input-depths))
        missing-layers (into {} (map vector missing-depths (repeat 0)))
        merged-layers (merge layers-from-input missing-layers)
        layers-as-mapsv (sort-by :depth
                                 (map #(hash-map :depth (first %) :range (second %)) merged-layers))]
    layers-as-mapsv))

(defn layer-scanner-pos
  ""
  [t layer]
  (let [range (:range layer)
        pos (if (zero? range)
              -1
              (mod t range))]
    (assoc layer :scanner-pos pos :time t)))

;
; Cheesy, but the way to get the starting positions is via t = -1.
;
(defn layer-scanner-positions-after-t
  ""
  [t layers]
  (map #(layer-scanner-pos t %) layers))

(defn layer-scanner-positions-seq
  ""
  [layers]
  (map #(layer-scanner-positions-after-t % layers)
       (range 0 (count layers))))

(defn scanner-position-severity
  ""
  [layer-scanner-position]
  (let [scanner-pos (:scanner-pos layer-scanner-position)
        severity (* (:depth layer-scanner-position) (:range layer-scanner-position))]
    (if (= 0 scanner-pos)
      severity
      0)))

(defn packet-positions
  ""
  [trip-seq]
  (let [indexes (range 0 (count (first trip-seq)))]
    (map #(get (vec %1) %2) trip-seq indexes)))

(defn trip-severity
  ""
  [trip-seq]
  (apply + (map scanner-position-severity (packet-positions trip-seq))))

(comment
  (def day13-layers
    (input->layers day13-input-str))
  (trip-severity (layer-scanner-positions-seq day13-layers)))

