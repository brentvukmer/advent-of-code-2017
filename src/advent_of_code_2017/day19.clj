(ns advent-of-code-2017.day19
  (:require [clojure.java.io :as io]))

;
; Part 1
;
;
; Initial direction: down
; Initial location: first valid-path-char
; While there's a next valid-path-char in the current direction, proceed.
; If the next char is a valid-path-char but doesn't match the last char, skip ahead one.
; Otherwise, locate the neighbor that hasn't already been seen, change the direction to face that neighbor, and proceed.
; If the current char is alphabetic, add it to the list, then move to the "next" char.
;

(defn read-input
  ""
  [input-name]
  (clojure.string/split-lines (slurp (io/resource input-name))))

(defn non-space-indexes
  ""
  [inputs]
  (for [i (range (count inputs))
        :let [line (get inputs i)]]
    (filter some? (map-indexed (fn [index item] (if (not (Character/isSpace item)) [[i index] item] nil)) line))))

(defn parse-diagram
  ""
  [inputs]
  (->> (non-space-indexes inputs)
       (apply concat)
       (into {})))


(def initial-direction [1 0])


(defn remaining-keys
  ""
  [curr keys]
  (remove #(= curr %) keys))


(defn next-using
  ""
  [curr direction diagram]
  (let [direction-next (mapv + curr direction)]
    (when (contains? diagram direction-next) direction-next)))


(defn letter?
  ""
  [c]
  (Character/isAlphabetic (int c)))


(defn index-distance
  ""
  [index1 index2]
  (reduce + (mapv #(Math/abs %) (mapv - index1 index2))))


(defn find-closest-index
  [curr remaining-keys]
  (get-in (vec (sort (group-by #(index-distance curr %) remaining-keys))) [0 1 0]))


(defn crossing?
  ""
  [c1 c2]
  (= #{\| \-} (set [c1 c2])))


(defn best-direction
  ""
  [curr direction next-step best-next]
  (if (nil? best-next) nil
                       (if (= next-step best-next)
                         direction
                         (mapv - best-next curr))))


(defn best-next
  [curr direction diagram remaining-keys]
  (let [next-step (next-using curr direction diagram)
        best-next (if (nil? next-step) (find-closest-index curr remaining-keys) next-step)
        updated-direction (best-direction curr direction next-step best-next)]
    [best-next updated-direction]))


(defn lazy-path
  ""
  ([diagram]
   (let [path-keys (sort (keys diagram))
         curr (first path-keys)
         direction [1 0]
         seen #{curr}
         remaining-keys (rest path-keys)]
     (lazy-path curr direction seen remaining-keys diagram)))

  ([curr direction seen remaining-keys diagram]
   (let [[next-step next-direction] (best-next curr direction diagram remaining-keys)]
     (lazy-seq (cons curr (lazy-path next-step next-direction (conj seen next-step) (remove #(= next-step %) remaining-keys) diagram))))))


(defn part1
  ""
  [diagram]
  (apply str
         (filter letter?
                 (map #(get diagram %)
                      (take-while some? (lazy-path diagram))))))

;
; PART 2
;

(defn part2
  ""
  [diagram]
  (count (take-while some? (lazy-path diagram))))