(ns advent-of-code-2017.day11
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

;
; Part 1
;

(def day11-input-str
  (slurp (io/resource "day11")))

(defn input-str->keywords
  [input-str]
  (vec (map keyword (clojure.edn/read-string (str "[" input-str "]")))))

;
; Fun reading on hex grids:
; - http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
; - https://www.redblobgames.com/grids/hexagons/
;

(def hex-unit-coords
  {:n  [0 1]
   :ne [1 0]
   :se [1 -1]
   :s  [0 -1]
   :sw [-1 0]
   :nw [-1 1]
   })

(defn xy->xyz
  [[x y]]
  (let [z (* -1 (+ x y))]
    [x y z]))

(defn hex-moves->xyz
  ""
  [input-str]
  (xy->xyz (vec (apply map + (map #(% hex-unit-coords) (input-str->keywords input-str))))))

(defn grid-dist
  ""
  ([xyz]
   (grid-dist [0 0 0] xyz))
  ([[x1 y1 z1] [x2 y2 z2]]
   (apply max (map math/abs [(- x2 x1) (- y2 y1) (- z2 z1)]))))

;
; Part 2
;

(defn hex-move->xy
  ""
  [xy hex-move]
  (vec (map + xy (hex-move hex-unit-coords))))

(defn hex-moves->xyzs
  ""
  [input-str]
  (let [moves (input-str->keywords input-str)
        xys (reductions hex-move->xy [0 0] moves)
        xyzs (map xy->xyz xys)]
    xyzs))

(defn max-dist-pos
  ""
  [input-str]
  (let [xyzs (hex-moves->xyzs input-str)
        sorted-by-dist (sort-by grid-dist xyzs)
        max-dist-pos (last sorted-by-dist)]
    max-dist-pos))

(defn dist-max-current
  ""
  [input-str]
  (grid-dist (hex-moves->xyz input-str) (max-dist-pos input-str)))