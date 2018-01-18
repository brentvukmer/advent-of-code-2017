(ns advent-of-code-2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;
; Part 1
;

(defn num-check?
  ""
  [input]
  (let [first-char (first input)]
    (and (some? first-char)
         (or (= \- first-char)
             (Character/isDigit first-char)))))

(defn get-keyword-or-number
  ""
  [input]
  (if (num-check? input)
    (Integer/parseInt input)
    (keyword input)))

(defn parse-instruction
  ""
  [input]
  {:op (keyword (first input))
   :x  (get-keyword-or-number (second input))
   :y  (get-keyword-or-number (get input 2))})

(def inputs
  (mapv #(str/split % #"\s")
        (str/split-lines
          (slurp
            (io/resource "day18")))))

(defn registers
  [instruction-inputs]
  (into {}
        (map #(vector (keyword %) 0)
             (set
               (filter #(Character/isAlphabetic
                          (int (first %)))
                       (map second instruction-inputs))))))

(def sample-inputs
  [["set" "a" "1"]
   ["add" "a" "2"]
   ["mul" "a" "a"]
   ["mod" "a" "5"]
   ["snd" "a"]
   ["set" "a" "0"]
   ["rcv" "a"]
   ["jgz" "a" "-1"]
   ["set" "a" "1"]
   ["jgz" "a" "-2"]])

(def instructions (mapv parse-instruction inputs))

(defn perform-instruction
  ""
  [registers history instruction]

  (println "processing: " instruction)
  (let [op (:op instruction)
        x (:x instruction)
        y (:y instruction)
        y-val (if (keyword? y)
                (get registers y)
                y)]
    (cond

      (= :snd op)
      {:instruction instruction :registers registers :sound (get registers x)}

      (= :set op)
      (let [updated-registers (assoc registers x y-val)]
        {:instruction instruction :registers updated-registers})

      (= :add op)
      (let [updated-value (+ (get registers x) y-val)
            updated-registers (assoc registers x updated-value)]
        {:instruction instruction :registers updated-registers})

      (= :mul op)
      (let [updated-value (* (get registers x) y-val)
            updated-registers (assoc registers x updated-value)]
        {:instruction instruction :registers updated-registers})

      (= :mod op)
      (let [updated-value (mod (get registers x) y-val)
            updated-registers (assoc registers x updated-value)]
        {:instruction instruction :registers updated-registers})

      (= :rcv op)
      (let [x-val (get registers x)
            freq (when (not (zero? x-val))
                   (some #(when (= :snd (get-in % [:instruction :op])) (:sound %))
                         (reverse history)))]
        {:instruction instruction :registers registers :recovery freq})

      (= :jgz op)
      (let [x-val (if (keyword? x)
                    (get registers x)
                    x)
            index-offset (if (> x-val 0)
                           (if (keyword? y)
                             (get registers y)
                             y)
                           0)]
        {:instruction instruction :registers registers :index-offset index-offset})
      ))
  )

(defn follow-instructions
  [registers instructions]
  (loop [history []
         registers registers
         instruction-index 0]
    (if (or (= (count instructions) instruction-index)
            (some? (:recovery (last history))))
      history
      (let [instruction (get instructions instruction-index)
            result (perform-instruction registers history instruction)
            index-offset (:index-offset result 1)
            updated-offset (if (zero? index-offset) 1 index-offset)
            updated-registers (:registers result)
            updated-index (+ instruction-index updated-offset)]
        (recur (conj history result)
               updated-registers
               updated-index)))))


;
; Part 2
;