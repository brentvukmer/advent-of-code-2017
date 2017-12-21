(ns advent-of-code-2017.day5
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

;
; Part 1
;

(defn read-lines-classpath-resource
  ""
  [name]
  (clojure.string/split-lines (clojure.string/trim (slurp (io/resource name)))))

(def day5-inputs
  (vec
    (map (fn [x] (Integer/parseInt x))
         (read-lines-classpath-resource "day5"))))

;  (0) 3  0  1  -3  - before we have taken any steps.
;  (1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
;  2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
;  2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
;  2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
;  2  5  0  1  -2  - jump 4 steps forward, escaping the maze.

(defn find-escape-count
  "docstring"
  [coll]
  (loop [num-moves 0
         current-index 0
         coll coll]
    (if (> current-index (dec (count coll)))
      num-moves
      (let [current-value (get coll current-index)]
        (recur (+ num-moves current-value)
               (+ current-index current-value)
               (assoc-in coll [current-index] (inc current-value)))))))

