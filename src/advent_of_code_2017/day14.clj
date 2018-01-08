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

(comment
  (do
    (def sample-binstrs (prefix->binstrs "flqrgnkx"))
    (def input-binstrs (prefix->binstrs "vbqugkhl"))))

;
; Part 2
;

(defn binstrs->grid
  ""
  [binstrs]
  (mapv vec binstrs))

(comment
  (do
    (def sample-grid (binstrs->grid sample-binstrs))
    (def input-grid (binstrs->grid input-binstrs))))

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

(defn discover-groups
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
        (set (conj (get-adjacent-coords coord grid) coord))))))

(comment

  (def groups1 (discover-groups [[\1 \0 \0 \1 \1 \0]
                                 [\1 \1 \0 \1 \0 \0]
                                 [\0 \0 \1 \0 \1 \0]
                                 [\0 \0 \0 \0 \0 \0]
                                 [\1 \1 \1 \1 \1 \1]]))

  )

(defn merge-overlapping-groups
  ""
  [grid]
  (loop [groups (discover-groups grid)
         merged-groups []]
    (if (empty? groups)
      merged-groups
      (let [group (first groups)
            matching-groups (set (keep #(when (some group %) %) (rest groups)))
            group-plus-matching (conj matching-groups group)
            merged (apply set/union group-plus-matching)
            pruned (remove #(some merged %) merged-groups)
            remaining (remove #(contains? group-plus-matching %) groups)]
        (recur remaining
               (conj pruned merged))))))

(comment
  (do
    (def sample-groups (merge-overlapping-groups sample-grid))
    (def input-groups (merge-overlapping-groups input-grid))))

(comment
  (do
    (def snake-grid [[\1 \0 \0 \1 \1 \1]
                     [\1 \1 \0 \1 \0 \0]
                     [\0 \1 \1 \1 \0 \0]
                     [\0 \0 \0 \1 \0 \0]
                     [\1 \1 \1 \1 \0 \1]])
    (def snake-groups (vec (merge-overlapping-groups snake-grid)))))


; Where two groups have any adjacent coordinates, merge the groups into one
(defn merge-adjacent-groups
  ""
  [])



