(ns advent-of-code-2017.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2017.day10 :as knot]))

;
; Part 1
;

(defn gen-inputs
  ""
  [prefix]
  (vec (map #(str prefix "-" %) (range 128))))

(defn hexch->binstr
  ""
  [s]
  (str/replace (format "%4s" (Integer/toBinaryString (Integer/parseInt s 16)))
               #"\s" "0"))

(defn knothash->binstr
  ""
  [kh]
  (apply str (map #(hexch->binstr (str %)) kh)))

(defn prefix->binstrs
  ""
  [prefix]
  (let [inputs (gen-inputs prefix)
        binstrs (map #(knothash->binstr (knot/knot-hash %)) inputs)]
    binstrs))

(comment
  (do
    (def sample-binstrs (prefix->binstrs "flqrgnkx"))
    (def input-binstrs (prefix->binstrs "vbqugkhl"))))


;
; Super slow
;

(defn binstrs->grid
  ""
  [binstrs]
  (mapv vec binstrs))

(comment
  (do
    (def sample-grid (binstrs->grid sample-binstrs))
    (def input-grid (binstrs->grid input-binstrs))))

(defn find-ones
  ""
  [grid]

  (vec
    (let [max-row (count grid)
          max-col (count (first grid))]
      (for [row (range max-row)
            col (range max-col)
            :let [coord [row col]
                  val (get-in grid coord)]
            :when (= \1 val)]
        coord))))

(defn count-occupied-slots
  ""
  [prefix]
  (let [binstrs (prefix->binstrs prefix)
        grid (binstrs->grid binstrs)
        occupied-coords (find-ones grid)]
    (count occupied-coords)))

(defn part1
  ""
  []
  (count-occupied-slots "vbqugkhl"))
;
; Part 2
;

(defn get-adjacent-coords
  ""
  [coord grid]
  (let [rows (set (range (count grid)))
        cols (set (range (count (first grid))))
        possible-adj (mapv #(mapv + coord %)
                           [[1 0] [0 1] [-1 0] [0 -1]])]
    (vec
      (filter (fn [[x y]] (and (contains? rows x)
                               (contains? cols y)
                               (= \1 (get-in grid [x y])))) possible-adj))))
(defn map-coords
  ""
  [grid]
  (into {}
        (for [coord (find-ones grid)
              :let [neighbors (get-adjacent-coords coord grid)]]
          [coord neighbors])))

(comment
  (do
    (def grid1 [[\1 \0 \0 \1 \1 \0]
                [\1 \1 \0 \1 \0 \0]
                [\0 \0 \1 \0 \1 \0]
                [\0 \0 \0 \0 \0 \0]
                [\1 \1 \1 \1 \1 \1]])

    (def snake-grid [[\1 \0 \0 \1 \1 \1]
                     [\1 \1 \0 \1 \0 \0]
                     [\0 \1 \1 \1 \0 \0]
                     [\0 \0 \0 \1 \0 \0]
                     [\1 \1 \1 \1 \0 \1]])
    ))

;
; Behold the sweetness: https://github.com/bhauman/advent-of-clojure-2016/blob/master/src/advent_of_clojure_2017/day12.clj
;

(defn group
  ""
  [indexed root]
  (into #{}
        (tree-seq (let [seen (atom #{})]
                    (fn [x] (when-not (@seen x)
                              (swap! seen conj x))))
                  indexed
                  root)))

(defn find-groups
  [indexed]
  (set
    (map #(group indexed %) (keys indexed))))
