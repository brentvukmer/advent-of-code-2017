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


(defn rotate
  "Rotate clockwise by 90 degrees."
  [pixels]
  (apply mapv #(vec (reverse %&)) pixels))


(defn flip
  "Flip either right-to-left (:x axis) or top-to-bottom (:y axis)."
  [pixels axis]
  (cond
    (= axis :x)
    (mapv #(vec (reverse %)) pixels)
    (= axis :y)
    (vec (reverse pixels))))


(defn rule-match-candidates
  "Create rule-match candidates via rotating and flipping the pixels."
  [pixels]
  (set
    (conj
      (vec (take 4 (iterate rotate pixels)))
      (flip pixels :x)
      (flip pixels :y))))


(defn size-matching-rules
  "Find rules whose :input-size matches the pixels square dimensions."
  [pattern rules]
  (filter #(= (count pattern) (:input-size %)) rules))


(defn find-matching-rule
  "Find rule that matches pixel square dimensions and pattern."
  [pattern rules]
  (let [candidates (rule-match-candidates pattern)]
    (first
      (filter
        #(contains? candidates (:from %))
        (size-matching-rules pattern rules)))))


(defn pixels->squares
  "Splits a pixels square into n x n squares."
  [n pixels]
  (apply map vector
         (map #(map vec (partition n %))
              pixels)))


(defn two->three
  ""
  [pixels rules]
  ; Break the pixels up into 2x2 squares
  (let [squares (pixels->squares 2 pixels)]
    ; For each 2x2 square:
    ; - Find the corresponding enhancement rule (flip or rotate as needed)
    ; - Use rule to convert 2x2 square into a 3x3 square
    ; Merge 3x3 squares into one
    squares))


(defn split-enhance-merge
  ""
  [pixels n rules]
  ; Break the pixels up into n x n squares.
  ; For each n x n square:
  ; - Find the corresponding enhancement rule (flip or rotate as needed)
  ; - Use rule to convert n x n square into an n+1 x n+1 square
  ; Merge n+1 x n+1 squares into one
  (let [squares (pixels->squares 3 pixels)
        transformed (map #(:to (find-matching-rule % rules)) squares)
        merged (apply map concat transformed)]
    merged))


(defn enhance
  ""
  [pixels rules]
  (let [size (count (first pixels))
        split-factor
        (cond
          (= 0 (mod size 2))
          2
          (= 0 (mod size 3))
          3)
        enhanced (split-enhance-merge pixels split-factor rules)]
    enhanced))

;
; Part 2
;

