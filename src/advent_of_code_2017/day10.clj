(ns advent-of-code-2017.day10
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(def day10-input-str
  (slurp (io/resource "day10")))

(def day10-part1-inputs
  (clojure.edn/read-string (str "[" day10-input-str "]")))

(def sample-nums [0, 1, 2, 3, 4])

(def sample-lengths [3, 4, 1, 5])

(def default-params [(range 0 256) 0 0])

;
; Deleted garbage code that used drop/take/cycle but not very well.
;

;
; Code borrowed from:
; https://github.com/mfikes/advent-of-code/blob/master/src/advent_2017/day_10.cljc#L9-L17
;


(defn make-knot
  ""
  [[xs current-pos skip-size] length]
  (let [rotated (drop current-pos (cycle xs))
        to-reverse (take length rotated)
        num-xs (count xs)
        all-after (take (- num-xs length) (drop length rotated))
        reversed-and-after (concat (reverse to-reverse) all-after)
        xs-spliced-with-reversed (->>
                                   (cycle reversed-and-after)
                                   (drop (- num-xs current-pos))
                                   (take num-xs) (vec))
        next-pos (mod (+ current-pos length skip-size) num-xs)
        next-skip-size (mod (inc skip-size) num-xs)]
    [xs-spliced-with-reversed
     next-pos
     next-skip-size]))


(defn round [lengths [xs current-pos skip-size]]
  (reduce make-knot [xs current-pos skip-size] lengths))

(defn knot-hash
  ""
  [lengths]
  (apply * (take 2 (first (round lengths default-params)))))

;
; Part 2
;

(defn ascii-inputs
  ""
  [input-str]
  (concat
    (map #(int %) input-str)
    '(17, 31, 73, 47, 23)))

(def day10-part2-inputs
  (ascii-inputs day10-input-str))

(defn sparse-hash
  [lengths [xs current-pos skip-size]]
  (first (round
           (take (* 64 (count lengths)) (cycle lengths))
           [xs current-pos skip-size])))

(defn dense-hash
  [sparse-hash]
  (map #(apply bit-xor %) (partition 16 sparse-hash)))

(defn hexify
  [dense-hash]
  (apply str (map #(format "%02x" %) dense-hash)))
