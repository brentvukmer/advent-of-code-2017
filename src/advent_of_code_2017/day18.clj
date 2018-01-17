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

(defn num-check?
  ""
  [input]
  (let [first-char (first input)]
    (or (= \- first-char)
        (Character/isDigit first-char))))

(defn process-instruction
  ""
  [history instruction]

  (let [result
        (let [registers (:registers (last history))
              op (keyword (first instruction))]
          (println "processing: " instruction)
          (cond

            (= :snd op)
            (let [register-key (keyword (second instruction))]
              {:registers registers :sound (register-key registers)})

            (= :set op)
            (let [x (keyword (second instruction))
                  y (get instruction 2)
                  val (if (num-check? y)
                        (Integer/parseInt y)
                        ((keyword y) registers))
                  updated-registers (assoc registers x val)]
              {:registers updated-registers})

            (= :add op)
            (let [x (keyword (second instruction))
                  y (get instruction 2)
                  val (if (num-check? y)
                        (Integer/parseInt y)
                        ((keyword y) registers))
                  updated-value (+ (x registers) val)
                  updated-registers (assoc registers x updated-value)]
              {:registers updated-registers})

            (= :mul op)
            (let [x (keyword (second instruction))
                  y (Integer/parseInt (get instruction 2))
                  updated-value (* (x registers) y)
                  updated-registers (assoc registers x updated-value)]
              {:registers updated-registers})

            (= :mod op)
            (let [x (keyword (second instruction))
                  y (get instruction 2)
                  val (if (num-check? y)
                        (Integer/parseInt y)
                        ((keyword y) registers))
                  updated-value (mod (x registers) val)
                  updated-registers (assoc registers x updated-value)]
              {:registers updated-registers})

            (= :rcv op)
            (let [x (keyword (second instruction))
                  val (x registers)
                  freq (when (not (zero? val))
                         (recover-last-sound instructions instruction x))]
              {:registers registers :recovery freq})

            (= :jgz op)
            (let [x (second instruction)
                  x-val (if (num-check? x)
                          (Integer/parseInt x)
                          ((keyword x) registers))
                  y (get instruction 2)
                  index-offset (if (> x-val 0) (Integer/parseInt y) 0)]
              {:registers registers :index-offset index-offset})
            ))]
    (conj history result)))

(defn reductions-instructions
  [registers instructions]
  (reduce
    (fn [history instruction] (process-instruction history instruction))
    [{:registers registers}]
    instructions))


;
; Part 2
;