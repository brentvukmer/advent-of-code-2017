(ns advent-of-code-2017.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;
; Diagnostics indicate that the local grid computing cluster has been contaminated with the Sporifica Virus.
; The grid computing cluster is a seemingly-infinite two-dimensional grid of compute nodes.
; Each node is either clean or infected by the virus.
;
; To prevent overloading the nodes (which would render them useless to the virus)
; or detection by system administrators, exactly one virus carrier moves through the network,
; infecting or cleaning nodes as it moves. The virus carrier is always located on a single node
; in the network (the current node) and keeps track of the direction it is facing.
;
; To avoid detection, the virus carrier works in bursts;
; in each burst, it wakes up, does some work, and goes back to sleep.
;
; The following steps are all executed in order one time each burst:
;
; - If the current node is infected, it turns to its right.
; -- Otherwise, it turns to its left. (Turning is done in-place; the current node does not change.)
; - If the current node is clean, it becomes infected.
; -- Otherwise, it becomes cleaned. (This is done after the node is considered for the purposes of changing direction.)
; - The virus carrier moves forward one node in the direction it is facing.
;
; Diagnostics have also provided a map of the node infection status (your puzzle input). Clean nodes are shown as .; infected nodes are shown as #.
; This map only shows the center of the grid; there are many more nodes beyond those shown, but none of them are currently infected.
;
;The virus carrier begins in the middle of the map facing up.

;
; Part 1
;


(def input (->> "day22" io/resource io/reader line-seq))


(def initial-grid (mapv vec input))


(def sample-grid (mapv vec (str/split-lines "..#\n#..\n...")))


(def sample-grid-bursts
  [[[[\. \. \#] [\# \. \.] [\. \. \.]] nil [1 1] [0 1]]
   [[[\. \. \#] [\# \# \.] [\. \. \.]] true [0 1] [-1 0]]
   [[[\. \. \#] [\# \. \.] [\. \. \.]] false []]
   []
   []
   []
   []])


(defn grid-dimensions
  [grid]
  [(count grid) (count (first grid))])


(def initial-grid-dimensions (grid-dimensions initial-grid))


(def initial-direction [-1 0])


(defn initial-position
  [grid]
  (let [dimensions (grid-dimensions grid)]
    [(quot (first dimensions) 2) (quot (second dimensions) 2)]))

;
; Direction vectors in terms of rows/cols:
; - up
; - right
; - down
; - left
;
(def direction-vectors
  [[-1 0] [0 1] [1 0] [0 -1]])


(defn infected?
  [position grid]
  (= \# (get-in grid position)))


(defn find-dv-idx
  [dv dvs]
  (ffirst
    (filter #(= dv (second %))
            (map-indexed (fn [idx x] [idx x]) dvs))))


(defn right-turn?
  [turn-direction]
  (= :right turn-direction))


(defn left-turn?
  [turn-direction]
  (= :left turn-direction))


(defn turn
  [turn-direction directionv]
  (let [idx (find-dv-idx directionv direction-vectors)
        num-dvs (count direction-vectors)
        next-direction (cond (left-turn? turn-direction)
                             (first (drop (dec (+ num-dvs idx)) (take (+ num-dvs idx) (cycle direction-vectors))))
                             (right-turn? turn-direction)
                             (first (drop (inc idx) (cycle direction-vectors)))
                             :else
                             :error-no-such-turn-direction)]
    next-direction))


(defn update-grid
  [infection-status position grid]
  (let [updated-val (if infection-status \# \.)
        updated-grid (assoc-in grid position updated-val)]
    [updated-grid infection-status]))


(defn expand-grid
  [grid]
  (let [[x _] (grid-dimensions grid)
        updated-x (+ 2 x)
        expanded-existing (mapv #(vec (cons \. (conj % \.))) grid)
        new-row (vec (repeat updated-x \.))
        updated-expanded (vec (cons new-row (conj expanded-existing new-row)))]
    updated-expanded))


(defn update-position-and-grid
  [position directionv grid]
  (let [[row-delta col-delta] directionv
        updated-position (mapv + position [row-delta col-delta])
        found (some? (get-in grid updated-position))]
    (if found
      [updated-position grid]
      (let [expanded-grid (expand-grid grid)
            shifted-prev-position (mapv inc position)
            shifted-updated-position (mapv + shifted-prev-position directionv)]
        [shifted-updated-position expanded-grid]))))


(defn burst
  [[grid _ position directionv]]
  (let [infected-status (infected? position grid)
        turn-direction (if infected-status :right :left)
        updated-directionv (turn turn-direction directionv)
        [infection-updated-grid updated-status] (update-grid (not infected-status) position grid)
        [updated-position position-updated-grid] (update-position-and-grid position updated-directionv infection-updated-grid)]
    [position-updated-grid updated-status updated-position updated-directionv]))


(defn do-bursts
  [n grid]
  (take (inc n)
        (iterate burst [grid nil (initial-position grid) initial-direction])))


(defn count-infection-bursts
  [n grid]
  (count
    (filter true?
            (map second
                 (do-bursts n grid)))))


(def part1-answer
  (count-infection-bursts 10000 initial-grid))


;
; Part 2
;

;
; Clean nodes become weakened.
; Weakened nodes become infected.
; Infected nodes become flagged.
; Flagged nodes become clean.
;


(def node-states {:clean \., :weakened \W, :infected \#, :flagged \F})


(def node-state-machine [:clean :weakened :infected :flagged])


; The virus carrier still functions in a similar way, but now uses the following logic during its bursts of action:
;
; Decide which way to turn based on the current node:
; If it is clean, it turns left.
; If it is weakened, it does not turn, and will continue moving in the same direction.
; If it is infected, it turns right.
; If it is flagged, it reverses direction, and will go back the way it came.
; Modify the state of the current node, as described above.
; The virus carrier moves forward one node in the direction it is facing.
;


(def node-state-turn-direction {:clean :left :weakened :straight :infected :right :flagged :reverse})


(defn node-state
  [position grid]
  (let [val (get-in grid position)
        node-state (ffirst (filter #(= (second %) val) node-states))]
    node-state))


(defn straight?
  [turn-direction]
  (= :straight turn-direction))


(defn reverse?
  [turn-direction]
  (= :reverse turn-direction))


(defn turn2
  [turn-direction directionv]
  (let [idx (find-dv-idx directionv direction-vectors)
        num-dvs (count direction-vectors)
        next-direction (cond (left-turn? turn-direction)
                             (first (drop (dec (+ num-dvs idx)) (take (+ num-dvs idx) (cycle direction-vectors))))
                             (right-turn? turn-direction)
                             (first (drop (inc idx) (cycle direction-vectors)))
                             (straight? turn-direction)
                             directionv
                             (reverse? turn-direction)
                             (mapv #(* -1 %) directionv)
                             :else
                             :error-no-such-turn-direction)]
    next-direction))


(defn next-node-state
  [node-state]
  (first
    (drop 1
          (drop-while #(not= % node-state)
                      (cycle node-state-machine)))))


(defn burst2
  [[grid _ position directionv]]
  (let [node-state (node-state position grid)
        turn-direction (node-state-turn-direction node-state)
        updated-directionv (turn2 turn-direction directionv)
        updated-node-state (next-node-state node-state)
        node-updated-grid (assoc-in grid position (get node-states updated-node-state))
        [updated-position position-updated-grid] (update-position-and-grid position updated-directionv node-updated-grid)]
    [position-updated-grid updated-node-state updated-position updated-directionv]))


(defn do-bursts2
  [n grid]
  (take (inc n)
        (iterate burst2 [grid nil (initial-position grid) initial-direction])))


(defn count-infection-bursts2
  [n grid]
  (count
    (filter #(= :infected %)
            (map second
                 (do-bursts2 n grid)))))


(comment
  (count-infection-bursts2 100 sample-grid)
  (count-infection-bursts2 10000000 sample-grid)
  (count-infection-bursts2 10000000 initial-grid))

