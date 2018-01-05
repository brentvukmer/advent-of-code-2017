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

(defn full-layer-ranges
  "Includes placeholders for depths where there is no layer; those are treated as zero-range (no-op) layers."
  [layers]
  (let [layer-ranges (map range (map :range layers))
        full-layer-ranges (map #(if (empty? %) '(-1) %) layer-ranges)]
    full-layer-ranges))

(defn gen-matrix-at
  "Creates a square matrix of scanner positions by layer for the given time t."
  [num-layers t lazy-scanner-position-seqs]
  (mapv #(vec (take num-layers (drop (* t num-layers) %))) lazy-scanner-position-seqs))

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

;
; Takes forever
;
(defn find-path-time
  ""
  [layers max-time]
  (let [layer-ranges (full-layer-ranges layers)
        lazy-scanner-position-seqs (mapv lazy-scanner-positions layer-ranges)
        num-layers (count layers)]
    (for [t (range max-time)
          :let [matrix (gen-matrix-at num-layers t lazy-scanner-position-seqs)
                diagonal (get-diagonal matrix)
                safe-path-found (safe-path? diagonal)]
          :when (or (= t max-time)
                    safe-path-found)]
      {:picoseconds       t
       :scanner-positions (if safe-path-found
                            diagonal
                            :path-not-found)})))


;
; mfikes code
; https://github.com/mfikes/advent-of-code/blob/master/src/advent_2017/day_13.cljc
;

(def input (->> "day13" io/resource io/reader line-seq))

;
; Don't need all my complicated code for generating matrix, finding diagonal.
; All the data needed is right there in the inputs.
;

(def data (->> input (map #(mapv read-string (re-seq #"\d+" %)))))

;
; (* 2 (dec range)) to cover the number of positions
; going from max range and then back to 1, starting over
; at 0 via the magic of mod.
;
; No need to use cycle.
;

(defn caught? [depth range]
  (zero? (mod depth (* 2 (dec range)))))

;
; Nice little example of using transducers.
; Should work if 'data' is a lazy sequence, right?
;
; Helpful example of keep/when.
;

(defn part-1 []
  (transduce (keep (fn [[depth range]]
                     (when (caught? depth range)
                       (* depth range))))
             + data))
;
; So simple, fed by a lazy sequence.
;
; Does that makes it non-terminating if
; the data doesn't provide for an answer, though?
;
; Helpful example of some/when.
;

(defn part-2 []
  (some (fn [delay]
          (when (not-any? (fn [[depth range]]
                            (caught? (+ depth delay) range))
                          data)
            delay))
        (range)))