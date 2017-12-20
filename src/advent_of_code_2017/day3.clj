(ns advent-of-code-2017.day3
  (:require [clojure.math.numeric-tower :as math]))

;
; Part 1
;

(defn starting-position
  ([]
   (starting-position #(inc (:value %))))
  ([val-fn]
   {:position [0 0] :value 1 :turn 0 :next-directions [] :val-fn val-fn}))

(defn gen-arm-pattern
  ""
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
   :turn     (:turn last-move)
   :val-fn   (:val-fn last-move)})

(defn get-next-directions
  ""
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
  ""
  [last-move]
  (let [next (if (empty? (get-next-directions last-move))
               (gen-next-arm-first-move last-move)
               (gen-arm-next-move last-move))]
    (assoc next :value ((:val-fn last-move) last-move))))

(defn gen-spiral-until
  ""
  [start stop-fn]
  (take-while stop-fn (iterate next-move start)))

(defn gen-spiral-until-equals
  ""
  [start val]
  (gen-spiral-until start #(< (:value %) (inc val))))

(defn manhattan-distance
  ""
  ([position]
   (+ (math/abs (first position)) (math/abs (second position))))
  ([pos2 pos1]
   (manhattan-distance (map - pos2 pos1))))

(defn manhattan-distance-at
  ""
  [val]
  (let [position (:position (last (gen-spiral-until-equals (starting-position) val)))]
    (manhattan-distance position)))
;
; Part 2
;

(defn gen-spiral-until-gt
  ""
  [start val]
  (gen-spiral-until start #(<= (:value %) val)))


(defn neighbors
  ""
  [spiral]
  (let [last-move (last spiral)
        previous-moves (drop-last spiral)]
    (filter #(= 1 (manhattan-distance (:position last-move) (:position %))) previous-moves)))

(defn zeros-spiral
  ""
  [n]
  (let [spiral (gen-spiral-until-equals (starting-position) n)]
    (cons (first spiral) (map #(assoc % :value 0) (rest spiral)))))

;
; Do this recursively:
;
; - Take the first item in the spiral
; - Loop/recur through the rest
; -- updated-values
; -- remaining
;
; Take the first item remaining, assoc :value (sum neighbors (conj update-values item)
; When remaining is empty, return updated values
;

(defn sum-neighbors
  ""
  [spiral]
  (apply + (map :value (neighbors spiral))))


