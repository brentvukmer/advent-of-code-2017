(ns advent-of-code-2017.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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

(defn count-occupied-slots
  ""
  [prefix]
  (let [binstrs (prefix->binstrs prefix)
        num-occupied-slots (apply + (map #(get (frequencies %) \1) binstrs))]
    num-occupied-slots))

(defn part1
  ""
  []
  (count-occupied-slots "vbqugkhl"))

(def sample-binstrs (prefix->binstrs "flqrgnkx"))

(def input-binstrs (prefix->binstrs "vbqugkhl"))

;
; Part 2
;

; Record coordinates for slots with a 1
;
; Process one row at a time
; Where x coordinates are adjacent, create a region
; Search previous row's coordinates, where y coordinates are adjacent consolidate regions

(defn binstrs->grid
  ""
  [binstrs]
  (mapv vec binstrs))

(defn get-adjacent-coords
  ""
  [coord grid]
  (let [xs (set (range (count (first grid))))
        ys (set (range (count grid)))
        possible-adj (mapv #(mapv + coord %)
                           [[1 0] [0 1] [-1 0] [0 -1]])]
    (vec
      (filter (fn [[x y]] (and (contains? xs x)
                               (contains? ys y))) possible-adj))))

(defn discover-regions
  ""
  [grid]
  (let [max-x (count (first grid))
        max-y (count grid)
        xy-region (atom {:region 0})]
    (for [x (range max-x)]
      (for [y (range max-y)
            :let [coord [x y]
                  val (get-in grid coord)
                  current-region (get @xy-region coord)
                  adjacent-coords (get-adjacent-coords coord grid)
                  adjacent-region (filter some? (map #(get @xy-region %) adjacent-coords))]
            :when (= \1 val)]
        (cond

          (and
            (nil? current-region)
            (nil? adjacent-region))

          (let [new-region (inc (get @xy-region :region))
                coords (conj adjacent-coords coord)]
            (do
              (swap! xy-region assoc :region new-region)
              (map #(swap! xy-region assoc % new-region) coords)))

          (and
            (nil? current-region)
            (some? adjacent-region))

          (swap! xy-region assoc coord adjacent-region)

          (and
            (some? current-region)
            (nil? adjacent-region))

          (map #(swap! xy-region assoc % current-region) adjacent-coords)

          )

        ))
    (apply sorted-set (vals @xy-region))))





