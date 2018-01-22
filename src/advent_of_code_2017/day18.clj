(ns advent-of-code-2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async
             :as async
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

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


(defn sound
  ""
  [registers x]
  {:registers registers :sound (get registers x)})


(defn recover
  ""
  [registers x history]
  (let [x-val (get registers x)
        freq (when (not (zero? x-val))
               (some #(when (= :snd (get-in % [:instruction :op])) (:sound %))
                     (reverse history)))]
    {:registers registers :recovery freq}))


(defn perform-instruction
  ""
  [registers history instruction snd-fn rcv-fn]

  (let [op (:op instruction)
        x (:x instruction)
        y (:y instruction)
        y-val (if (keyword? y)
                (get registers y)
                y)]
    (cond

      (= :snd op)
      (assoc (snd-fn registers x) :instruction instruction)

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
      (assoc (rcv-fn registers x history) :instruction instruction)

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
  [registers instructions snd-fn rcv-fn]
  (loop [history []
         registers registers
         instruction-index 0]
    (if (or (= (count instructions) instruction-index)
            (some? (:recovery (last history))))
      history
      (let [instruction (get instructions instruction-index)
            result (perform-instruction registers history instruction snd-fn rcv-fn)
            index-offset (:index-offset result 1)
            updated-offset (if (zero? index-offset) 1 index-offset)
            updated-registers (:registers result)
            updated-index (+ instruction-index updated-offset)]
        (recur (conj history result)
               updated-registers
               updated-index)))))

(comment
  (follow-instructions (registers inputs) (mapv parse-instruction inputs) sound recover))

;
; Part 2
;

(defn send-fn
  ""
  [out registers x]
  (let [key-or-val (get-keyword-or-number x)
        val (if (keyword? key-or-val)
              (get registers key-or-val)
              key-or-val)
        result-channel (go (>! out val))
        send-result (<!! result-channel)]
    {:channel out :val val :send-result send-result}))

(defn receive-fn
  ""
  [in registers x _]
  (let [key-or-val (get-keyword-or-number x)
        val (if (keyword? key-or-val)
              (get registers key-or-val)
              key-or-val)
        result-channel (go (let [[v ch] (alts! in (timeout 500))] {:value v :channel ch}))
        receive-result (<!! result-channel)]
    {:channel in :val val :receive-result receive-result}))

(defn follow-instructions-tandem
  [inputs]
  (let [regs (registers inputs)
        instructions (mapv parse-instruction inputs)
        c0 (chan)
        c1 (chan)
        reg0 (assoc regs :p 0)
        reg1 (assoc regs :p 1)
        p0 (partial (fn [in out regs commands]
                      (follow-instructions regs commands (partial send-fn out) (partial receive-fn in)))
                    c1 c0 reg0)
        p1 (partial (fn [in out regs commands]
                      (follow-instructions regs commands (partial send-fn out) (partial receive-fn in)))
                    c0 c1 reg1)
        result-channel-0 (go (p0 instructions))
        result-channel-1 (go (p1 instructions))]

    { :p0 (<!! result-channel-0) :p1 (<!! result-channel-1)}))
