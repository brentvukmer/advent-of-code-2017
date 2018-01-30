(ns advent-of-code-2017.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def start-pattern-input ".#.\n..#\n###")

(def start-pattern (mapv vec (str/split-lines start-pattern-input)))


;
; Part 1
;


(defn parse-rule-input
  ""
  [rule-input]
  (let [parsed
        (mapv
          #(mapv vec (str/split % #"\/"))
          (str/split rule-input #"\s=>\s"))
        from (first parsed)
        to (second parsed)]
    {:input-size (count from)
     :from       from
     :to         to}))


(defn parse-rules-from
  ""
  [input-name]
  (mapv parse-rule-input
        (str/split-lines
          (slurp
            (io/resource input-name)))))


(defn rotate-clockwise
  "Rotate clockwise by 90 degrees."
  [pixels]
  (apply mapv #(vec (reverse %&)) pixels))


(defn rotations
  "Rotate clockwise through 360 degrees, then counterclockwise through 360 degrees."
  [pixels]
  (set (take 4 (iterate rotate-clockwise pixels))))


(defn flip
  "Flip either right-to-left (:x axis) or top-to-bottom (:y axis)."
  [axis pixels]
  (cond
    (= axis :x)
    (mapv #(vec (reverse %)) pixels)
    (= axis :y)
    (vec (reverse pixels))))


(defn rotate-flip-set
  ""
  [pattern]
  (let [rotated (rotations pattern)
        flip-xs (map #(flip :x %) rotated)
        flip-ys (map #(flip :y %) rotated)
        flip-xys (map #(flip :y %) flip-xs)
        flip-yxs (map #(flip :x %) flip-ys)
        flips (concat flip-xs flip-ys flip-xys flip-yxs)
        candidates (into rotated (concat flips (rotations flips)))]
    candidates))


(defn size-matching-rules
  "Find rules whose :input-size matches the pixels square dimensions."
  [pattern rules]
  (filter #(= (count pattern) (:input-size %)) rules))


(defn find-matching-rule
  "Find rule that matches pixel square dimensions and pattern."
  [pattern rules]
  (let [candidates (rotate-flip-set pattern)]
    (first
      (filter
        #(contains? candidates (:from %))
        (size-matching-rules pattern rules)))))

(defn split-factor
  ""
  [c]
  (let [size (count c)
        split-factor
        (cond
          (= 0 (mod size 2))
          2
          (= 0 (mod size 3))
          3)]
    split-factor))

(defn split
  "Splits a pixels square into n x n squares."
  [pixels]
  ; Account for starting pixels square being divisible by 3 (so already split)
  (let [n (split-factor pixels)]
    (if (= n (count pixels))
      (vector (vector pixels))
      (mapv
        #(apply mapv vector
                (mapv (fn [x]
                        (map vec (partition n x))) %))
        (partition n pixels)))))

(defn join
  "Joins n x n squares back into a single pixels square."
  [enhanced]
  (let [n (split-factor enhanced)]
    ; Account for starting pixels square being divisible by 3 (so already split)
    (if (= 1 (count enhanced))
      (first enhanced)
      (mapv vec
            (apply concat
                   (map #(apply map concat %)
                        (partition n enhanced)))))))

(defn enhance
  ""
  [pixels rules]
  (let [squares (split pixels)
        enhanced (map #(:to (find-matching-rule % rules)) (apply concat squares))
        merged-squares (join enhanced)]
    merged-squares))


(defn part1
  ""
  [n rules]
  (count
    (filter #(= \# %)
            (flatten
              (last
                (take (inc n)
                      (iterate #(enhance % rules) start-pattern)))))))


(comment
  (part1 2 (parse-rules-from "day21-sample"))
  (part1 5 (parse-rules-from "day21")))

;
; Part 2
;
