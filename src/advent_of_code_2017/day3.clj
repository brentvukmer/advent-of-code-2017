(ns advent-of-code-2017.day3
  (:require [clojure.math.numeric-tower :as math]))

;
; Part 1
;

(def starting-position
  {:position [0 0] :value 1 :turn 0 :next-directions []})

(defn gen-arm-pattern
  [n c1 c2 c3]
  (vec (cons c1 (concat (repeat n c2) (repeat n c3)))))

(defn gen-rul
  "Generate a RUL pattern."
  [n]
  (gen-arm-pattern n :R :U :L))

(defn gen-ldr
  "Generate a LDR pattern."
  [n]
  (gen-arm-pattern n :L :D :R))

(defn gen-arm
  "Generate an LDR pattern if even, an RUL if odd."
  [n]
  (if (odd? n) (gen-rul n) (gen-ldr n)))

(def direction->xy {:R [1, 0] :L [-1, 0] :U [0, 1] :D [0, -1]})

(defn direction->move
  "docstring"
  [last-move direction]
  {:position (vec (map + (:position last-move) (direction->xy direction)))
   :value    (inc (:value last-move))
   :turn     (:turn last-move)})

(defn get-next-directions
  [move]
  (:next-directions move))

(defn gen-next-arm-first-move
  "Generate the first move for the next arm in the spiral."
  [last-move]
  (let [next-turn (inc (:turn last-move))
        next-directions (gen-arm next-turn)
        next-move (direction->move last-move (first next-directions))]
    (assoc next-move :turn next-turn :next-directions (rest next-directions))))

(defn gen-arm-next-move
  "Generate the next move for the current arm in the spiral."
  [last-move]
  (let [next-directions (get-next-directions last-move)
        next-move (direction->move last-move (first next-directions))]
    (assoc next-move :next-directions (rest next-directions))))

(defn next-move
  [last-move]
  (if (empty? (get-next-directions last-move))
    (gen-next-arm-first-move last-move)
    (gen-arm-next-move last-move)))

(defn update-spiral-by-move
  "docstring"
  [spiral direction]
  (let [last-move (last spiral)
        next-move (direction->move last-move direction)]
    (conj spiral next-move)))

(defn gen-spiral-until
  [val]
  (take-while #(< (:value %) val) (iterate next-move starting-position)))

(defn manhattan-distance
  [position]
  (+ (math/abs (first position)) (math/abs (second position))))

;
; Part 2
;

