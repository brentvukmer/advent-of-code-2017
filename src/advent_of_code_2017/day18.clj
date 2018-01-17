(ns advent-of-code-2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;
; Part 1
;

; List of instructions
; List of registers - have to be able to move backwards and forwards
;

(def instructions
  (mapv #(str/split % #"\s")
        (str/split-lines
          (slurp
            (io/resource "day18")))))

(def registers
  (into {}
        (map #(vector (keyword %) 0)
             (set
               (filter #(Character/isAlphabetic
                          (int (first %)))
                       (map second instructions))))))

(defn recover-last-sound
  "If x is not zero, finds the most recent snd instruction with a non-zero frequency argument, and returns that frequency argument."
  [instructions instruction x]
  ; Find index of instruction
  ; Create subvec 0 index
  ; Reverse subvec
  ; Search list for snd w/ non-zero arg
  ; Return arg if found
  )

(defn process-instruction
  ""
  [registers instruction]
  (let [op (first instruction)]
    (cond

      (= "snd" op)
      (let [register-key (keyword (second instruction))]
        {:sound (register-key registers)})

      (= "set" op)
      (let [x-key (keyword (second instruction))
            y-key (keyword (get instruction 2))
            updated-value (y-key registers)
            updated-registers (assoc registers x-key updated-value)]
        {:registers updated-registers})

      (= "add" op)
      (let [x-key (keyword (second instruction))
            y-key (keyword (get instruction 2))
            updated-value (+ (x-key registers) (y-key registers))
            updated-registers (assoc registers x-key updated-value)]
        {:registers updated-registers})

      (= "mul" op)
      (let [x-key (keyword (second instruction))
            y-key (keyword (get instruction 2))
            updated-value (* (x-key registers) (y-key registers))
            updated-registers (assoc registers x-key updated-value)]
        {:registers updated-registers})

      (= "mod" op)
      (let [x-key (keyword (second instruction))
            y-key (keyword (get instruction 2))
            updated-value (mod (x-key registers) (y-key registers))
            updated-registers (assoc registers x-key updated-value)]
        {:registers updated-registers})

      (= "rcv" op)
      (let [x (Integer/parseInt (second instruction))
            freq (when (not (zero? x))
                   (recover-last-sound instructions instruction x))]
        {:recovery freq})

      (= "jgz" op)
      (let [index-update (Integer/parseInt (second instruction))]
        [registers index-update]))))

(defn process-instruction-at
  ""
  [registers instructions index]
  (let [instruction (get instructions index)
        [updated-registers index-update] (process-instruction registers instruction)]
    [updated-registers (+ index index-update)]))

;
; Part 2
;