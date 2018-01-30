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

(defn join-factor
  ""
  [enhanced]
  (int (Math/sqrt (* (count enhanced) (count (first enhanced))))))


(defn square?
  [matrix]
  (= (count matrix)
     (count (first matrix))))

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

(defn join2
  ""
  [enhanced]
  (let [flattened (flatten enhanced)
        size (int (Math/sqrt (count flattened)))]
    (mapv vec (partition size flattened))))


(defn enhance
  ""
  [pixels rules]
  (let [squares (split pixels)
        enhanced (map #(:to (find-matching-rule % rules)) (apply concat squares))
        merged-squares (join enhanced)]
    merged-squares))

(defn do-turns
  ""
  [n rules]
  (take (inc n)
        (iterate #(enhance % rules) start-pattern)))

(defn count-pixels
  ""
  [results]
  (let [final (last results)
        flattened (flatten final)
        freqs (frequencies flattened)
        num-pixels (get freqs \#)]
     num-pixels))


(comment
  (count-pixels (do-turns 2 (parse-rules-from "day21-sample")))
  (count-pixels (do-turns 5 (parse-rules-from "day21"))))

;
; Part 2
;

(defn enhance2
  ""
  [pixels rules]
  (let [squares (split pixels)
        enhanced (map #(:to (find-matching-rule % rules)) (apply concat squares))
        merged-squares (join2 enhanced)]
    merged-squares))

(defn do-turns2
  ""
  [n rules]
  (take (inc n)
        (iterate #(enhance2 % rules) start-pattern)))

(comment
  (def results2 (do-turns2 18 (parse-rules-from "day21")))
  (count-pixels results2))