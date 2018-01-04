(ns advent-of-code-2017.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

;
; Part 1
;

(def day13-input-str
  (slurp (io/resource "day13")))

(def sample-input-str "0: 3\n1: 2\n4: 4\n6: 4")

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
  (let [line-tokens (input->line-tokens input-str)
        layers-from-input (into {} (map line-tokens->map-entry line-tokens))
        input-depths (sort (map first layers-from-input))
        all-depths (range
                     (inc (last input-depths)))
        missing-depths (set/difference
                         (set all-depths)
                         (set input-depths))
        missing-layers (into {} (map vector missing-depths (repeat 0)))
        merged-layers (merge layers-from-input missing-layers)
        layers-as-mapsv (sort-by :depth
                                 (map #(hash-map :depth (first %) :range (second %)) merged-layers))]
    layers-as-mapsv))

(defn trip-positions
  ""
  ([positions-range num-layers num-rounds]
   (take (* num-layers num-rounds)
         (cycle
           (concat
             positions-range
             (drop-last (drop 1 (reverse positions-range)))))))
  ([positions-range num-layers]
   (trip-positions positions-range num-layers 1)))

(defn layer-history
  ""
  [layer num-layers]
  (let [positions-range (range (:range layer))
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
  (def sample-round1 (trip-seq (input->layers sample-input-str)))
  (def round1-seq (trip-seq (input->layers day13-input-str)))
  (def round1-severity (trip-severity round1-seq)))

;
; Part 2
;

(defn lazy-scanner-positions
  ""
  [positions-range]
  (cycle
    (concat
      positions-range
      (drop-last (drop 1 (reverse positions-range))))))

(defn gen-matrix-at
  "Creates a square matrix of scanner positions by layer for the given time t."
  [layers t]
  (let [num-layers (count layers)
        layer-ranges (map range (map :range layers))
        full-layers-ranges (map #(if (empty? %) '(-1) %) layer-ranges)
        scanner-positions (mapv #(vec (take num-layers
                                            (drop (* t num-layers)
                                                  (lazy-scanner-positions %)))) full-layers-ranges)]
    scanner-positions))

(defn diagonal-coords
  ""
  [matrix]
  (let [num-layers (count matrix)
        diagonal-coords (vec
                          (for [x (range num-layers)
                                y (range num-layers)
                                :when (= x y)]
                            [x y]))]
    diagonal-coords))

(defn get-diagonal
  ""
  [matrix]
  (let [diagonal-coords (diagonal-coords matrix)
        diagonals (vec (mapv #(get-in matrix %) diagonal-coords))]
    diagonals))

(defn safe-path?
  ""
  [path]
  (not (contains? (set path) 0)))

(defn find-path-time
  ""
  [layers max-time]
  (for [t (range max-time)
        :let [matrix (gen-matrix-at layers t)
              diagonal (get-diagonal matrix)
              safe-path-found (safe-path? diagonal)]
        :when (or (= t max-time)
                  safe-path-found)]
    {:picoseconds       t
     :scanner-positions (if safe-path-found
                          diagonal
                          :path-not-found)}))