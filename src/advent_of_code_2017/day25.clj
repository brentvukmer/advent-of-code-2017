(ns advent-of-code-2017.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;
; Part 1
;


(def input (slurp (io/resource "day25")))


(def sample-input (slurp (io/resource "day25-sample")))


(defn stanzas
  [input-str]
  (str/split input-str #"\n\n"))


(def sample-stanzas (stanzas sample-input))


(defn parse-state
  [begin-input]
  (keyword (str (last (drop-last begin-input)))))


(defn parse-value
  [input-line]
  (Integer/parseInt (apply str
                           (drop-last
                             (last
                               (str/split input-line #"\s"))))))


(defn parse-beginning
  [input-str]
  (let [inputs (str/split-lines input-str)
        begin-input (first inputs)
        begin-state (parse-state begin-input)
        perform-input (second inputs)
        num-steps (parse-value (str/replace perform-input " steps." "."))]
    {:begin-state begin-state :num-steps-until-checksum num-steps}))


(defn parse-direction
  [direction-line]
  (let [direction-words (str/split direction-line #"\s")
        direction (apply str (drop-last (last direction-words)))]
    (cond
      (= "right" direction)
      :R
      (= "left" direction)
      :L
      :else
      :error-no-such-move-direction)))


(defn parse-state-value-rules
  [input-lines]
  (let [current-value (parse-value (first input-lines))
        write-value (parse-value (second input-lines))
        move-direction (parse-direction (nth input-lines 2))
        next-state (parse-state (last input-lines))]
    {:current-value  current-value
     :write-value    write-value
     :move-direction move-direction
     :next-state     next-state}))


(defn parse-state-rules
  [input-str]
  (let [lines (str/split-lines input-str)
        state-input (first lines)
        state (parse-state state-input)
        [zero-block one-block] (split-at 4 (rest lines))
        zero-rules (parse-state-value-rules zero-block)
        one-rules (parse-state-value-rules one-block)]
    {:state state :zero-rules zero-rules :one-rules one-rules}))


(defn parse-instructions
  [stanzas]
  (let [beginning (parse-beginning (first stanzas))
        state-rules (map parse-state-rules (rest stanzas))]
    {:init beginning :state-rules state-rules}))

;
; Part 2
;



