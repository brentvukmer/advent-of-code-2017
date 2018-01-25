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


(defn read-inputs
  ""
  [resource-name]
  (mapv #(str/split % #"\s")
        (str/split-lines
          (slurp
            (io/resource resource-name)))))

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



(defn registers
  [instruction-inputs]
  (into {}
        (map #(vector (keyword %) 0)
             (set
               (filter #(Character/isAlphabetic
                          (int (first %)))
                       (map second instruction-inputs))))))

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
                y)
        result
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
          )]
    (assoc result
      :instruction instruction
      :time (. System currentTimeMillis)
      :thread (.getName (Thread/currentThread))))
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

  (def part1-results (let [inputs (read-inputs "day18")] (follow-instructions (registers inputs) (mapv parse-instruction inputs) sound recover))))

;
; Part 2
;

(defn send-fn
  ""
  [out registers x]
  (let [val (if (keyword? x)
              (get registers x)
              x)]
    (>!! out val)
    {:channel (:pid registers) :sent val :registers registers}))

(defn receive-fn
  ""
  [in registers x _]
  (let [[v _] (alts!! [in (timeout 1000)])
        updated-registers (if (some? v)
                            (assoc registers x v)
                            registers)]
    {:channel (:pid registers) :received v :registers updated-registers}))

(defn follow-instructions2
  [pid registers instructions snd-fn rcv-fn]
  (loop [history []
         registers (assoc registers :p pid :pid pid)
         instruction-index 0]

    (if (or (= (count instructions) instruction-index)
            (and
              (some? (last history))
              (contains? (last history) :received)
              (nil? (:received (last history)))))
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

(defn follow-instructions-tandem
  [inputs]
  (let [regs (registers inputs)
        instructions (mapv parse-instruction inputs)
        c0 (chan 100)
        c1 (chan 100)
        results-0 (go (follow-instructions2 0 regs instructions (partial send-fn c1) (partial receive-fn c0)))
        results-1 (go (follow-instructions2 1 regs instructions (partial send-fn c0) (partial receive-fn c1)))]

    {:p0 (<!! results-0) :p1 (<!! results-1)}))


(comment

  (def day18-part2-sample
    (let [inputs (read-inputs "day18-part2-sample")]
      (follow-instructions-tandem inputs)))

  (def day18-part2
    (count
      (filter #(contains? % :sent)
              (:p1
                (let [inputs (read-inputs "day18")]
                  (follow-instructions-tandem inputs)))))))
