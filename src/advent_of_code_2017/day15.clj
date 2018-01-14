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

(defn lazy-gen
  ""
  [start factor div]
  (iterate (fn [x] (rem (* x factor) div)) start))

(defn gen-pairs
  [n a' b']
  (partition 2
             (interleave
               (drop 1 (take (inc n) (lazy-gen a' factor-a divisor)))
               (drop 1 (take (inc n) (lazy-gen b' factor-b divisor))))))

(defn test-last-16-bits
  [num]
  (map (fn [x] (bit-test num x)) (range 16)))

(defn matching-pairs
  [n a' b']
  (filter #(= (test-last-16-bits (first %))
              (test-last-16-bits (second %)))
          (gen-pairs n a' b')))