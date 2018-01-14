(ns advent-of-code-2017.day15
  (:require [clojure.string :as str]))

;
; Part 1
;

(def start-a 591)

(def start-b 393)

(def factor-a 16807)

(def factor-b 48271)

(def divisor 2147483647)

(defn gen-next
  ""
  [x factor div]
  (rem (* x factor) div))

(defn lazy-gen
  ""
  [start factor div]
  (iterate (fn [x] (gen-next x factor div)) start))

(defn lazy-gen-pairs
  [a' b']
  (partition 2
             (interleave
               (lazy-gen a' factor-a divisor)
               (lazy-gen b' factor-b divisor))))

(defn test-last-16-bits
  [num]
  (map (fn [x] (bit-test num x)) (range 16)))

(defn pair-matches?
  ""
  [a b]
  (= (test-last-16-bits a)
     (test-last-16-bits b)))

(defn lazy-matching-pairs
  [a' b']
  (filter #(pair-matches? (first %) (second %)) (lazy-gen-pairs a' b')))

(defn reduce-pairs
  ""
  [n a' b']

  (ffirst
    (drop n
          (take (inc n)
                (iterate (fn [[sum prev-a prev-b]]
                           (let [next-a (gen-next prev-a factor-a divisor)
                                 next-b (gen-next prev-b factor-b divisor)
                                 matching (pair-matches? next-a next-b)
                                 next-sum (if matching (inc sum) sum)]
                             [next-sum next-a next-b]))
                         [0 a' b'])))))

(defn part1
  ""
  [a' b']
  (reduce-pairs 40000000 a' b'))