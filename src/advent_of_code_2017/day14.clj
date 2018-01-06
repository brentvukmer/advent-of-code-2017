(ns advent-of-code-2017.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2017.day10 :as knot]))

;
; Part 1
;

(def day14-input-str
  (slurp (io/resource "day14")))

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

(defn part1
  ""
  [prefix]
  (let [binstrs (prefix->binstrs prefix)
        num-occupied-slots (apply + (map #(get (frequencies %) \1) binstrs))]
    num-occupied-slots))

(def sample-binstrs (prefix->binstrs "flqrgnkx"))

(def input-binstrs (prefix->binstrs "vbqugkhl"))

;
; Part 2
;

; Process one row at a time
; Record coordinates for slots with a 1
; Where x coordinates are adjacent, create a region
; Search previous row's coordinates, where y coordinates are adjacent consolidate regions

(def sample-coords [[1 0 1 0]
                    [0 1 1 0]
                    [1 0 0 1]
                    [0 1 1 0]])

(def binstrs->coords
  ""
  [binstrs]
  (mapv
    #(mapv (fn [x] (Character/digit x 2)) %)
    binstrs))

(def occupied-coords
  ""
  [coords]
  (for [x (range (count (first coords)))
        y (range (count coords))
        :let [coord [x y]
              val (get-in coords coord)]
        :when (= 1 val)]
    coord))