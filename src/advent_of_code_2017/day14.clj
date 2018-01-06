(ns advent-of-code-2017.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2017.day10 :as knot]))

;
; Part 1
;

(def day14-input-str
  (slurp (io/resource "day14")))

(defn gen-inputs
  ""
  [prefix]
  (vec (map #(str prefix "-" %) (range 128))))

(def sample-inputs (gen-inputs "flqrgnkx"))

(defn kh-bigint
  ""
  [input-str]
  (BigInteger. (knot/knot-hash input-str) 16))

(defn hexch->binstr
  ""
  [s]
  (str/replace (format "%4s" (Integer/toBinaryString (Integer/parseInt s 16)))
               #"\s" "0"))

(defn knothash->binstr
  ""
  [kh]
  (apply str (map #(hexch->binstr (str %)) kh)))

(defn part1
  ""
  [prefix]
  (let [inputs (gen-inputs prefix)
        binstrs (map #(knothash->binstr (knot/knot-hash %)) inputs)
        occupied-slots (apply + (map #(get (frequencies %) \1) binstrs))]
    occupied-slots))