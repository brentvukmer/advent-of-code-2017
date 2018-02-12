(ns advent-of-code-2017.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; You find the Turing machine blueprints (your puzzle input)
; on a tablet in a nearby pile of debris. Looking back up at
; the broken Turing machine above, you can start to identify its parts:
;
; - A tape which contains 0 repeated infinitely to the left and right.
; - A cursor, which can move left or right along the tape and read or write values at its current position.
;-  A set of states, each containing rules about what to do based on the current value under the cursor.
;
; Each slot on the tape has two possible values:
;   - 0 (the starting value for all slots)
;   - 1
;
; Based on whether the cursor is pointing at a 0 or a 1, the current state says what value to write at the current position of the cursor, whether to move the cursor left or right one slot, and which state to use next.
;

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
    {:state state :rules [zero-rules one-rules]}))


(defn parse-instructions
  [stanzas]
  (let [beginning (parse-beginning (first stanzas))
        state-rules (map parse-state-rules (rest stanzas))]
    {:init beginning :state-rules state-rules}))


(defn update-cursor
  [cursor move-direction]
  (cond
    (= :L move-direction)
    (dec cursor)
    (= :R move-direction)
    (inc cursor)
    :else
    (throw (RuntimeException. "Invalid move direction"))))


(defn extend-tape
  [tape cursor]
  (if (nil? (get tape cursor))
    ;; Extend tape
    (cond
      (neg? cursor)
      [(vec (cons 0 tape)) 0]
      (pos? cursor)
      [(conj tape 0) cursor]
      :else
      (throw (RuntimeException. "Invalid cursor update")))
    tape))


(defn do-step
  [[tape cursor state state-rules]]
  (println "Doing step with cursor: " cursor)
  (let [rules (:rules (first (filter #(= state (:state %)) state-rules)))
        current-value (get tape cursor)
        rule-matching-value (first (filter #(= current-value (:current-value %)) rules))
        updated-value (:write-value rule-matching-value)
        updated-tape (assoc tape cursor updated-value)
        move-direction (:move-direction rule-matching-value)
        updated-cursor (update-cursor cursor move-direction)
        [extended-tape extended-cursor] (extend-tape updated-tape updated-cursor)
        updated-state (:next-state rule-matching-value)
        step-output [extended-tape extended-cursor updated-state state-rules]]
    (println "Step output: " step-output)
    step-output))


(defn process-instructions
  [instructions]
  (let [tape [0]
        cursor 0
        state (get-in instructions [:init :begin-state])
        state-rules (:state-rules instructions)
        num-steps (get-in instructions [:init :num-steps-until-checksum])]
    (take num-steps (iterate do-step [tape cursor state state-rules]))))


(defn part1
  [instructions]
  (reduce + (first (last (process-instructions instructions)))))


;
; Part 2
;


