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
  [t positions layer]
  (let [range (:range layer)
        start (if (zero? range)
                0
                (get positions t))
        end (if (zero? start)
              0
              (get positions (inc t)))
        scanner-pos {:start start :end end}]
    (assoc layer :scanner-pos scanner-pos :time t)))

(defn positions-range
  ""
  [layer]
  (let [layer-range (:range layer)]
    (if (zero? layer-range)
      '()
      (range 0 layer-range))))

(defn trip-positions
  ""
  [positions-range num-layers]
  (take num-layers
        (cycle
          (concat
            positions-range
            (drop-last (drop 1 (reverse positions-range)))))))

(defn layer-history
  ""
  [layer num-layers]
  (let [positions-range (positions-range layer)
        trip-positions (if (empty? positions-range)
                         '()
                         (trip-positions positions-range num-layers))
        history (if (empty? trip-positions)
                  (take num-layers (repeat layer))
                  (map #(assoc layer :scanner-pos %) trip-positions))]
    history))

(defn trip-seq
  ""
  [layers]
  (let [num-layers (count layers)]
    (apply map vector
           (map #(layer-history % num-layers) layers))))

(defn packet-positions
  ""
  [trip-seq]
  (let [indexes (range 0 (count (first trip-seq)))]
    (map #(get (vec %1) %2) trip-seq indexes)))

(defn severity-score
  ""
  [position]
  (let [start-pos (:scanner-pos position)
        severity (* (:depth position) (:range position))]
    (cond
      (nil? start-pos)
      0
      (zero? start-pos)
      severity
      :else
      0)))

(defn trip-severity
  ""
  [trip-seq]
  (apply + (map severity-score (packet-positions trip-seq))))

(comment
  (def day13-layers
    (input->layers day13-input-str))
  (trip-severity (layer-scanner-positions-seq day13-layers)))

