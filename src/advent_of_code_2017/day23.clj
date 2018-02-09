(ns advent-of-code-2017.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2017.day18 :as duet]))


;
; Part 1
;

; You decide to head directly to the CPU and fix the printer from there.
; As you get close, you find an experimental coprocessor doing so much work
; that the local programs are afraid it will halt and catch fire. This would
; cause serious issues for the rest of the computer, so you head in and see what you can do.
;
; The code it's running seems to be a variant of the kind you saw recently on that tablet. (Day 18)
; The general functionality seems very similar, but some of the instructions are different:
;
;    set X Y sets register X to the value of Y.
;    sub X Y decreases register X by the value of Y.
;    mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
;    jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
; Only the instructions listed above are used.
;
; The eight registers here, named a through h, all start at 0.
;


(def instructions (mapv duet/parse-instruction (duet/read-inputs "day23")))


(def registers (into {}
                     (mapv
                       #(vector (keyword (str (char %))) 0)
                       (range (int \a) (int \i)))))


(defn perform-instruction
  ""
  [registers instruction]

  (let [op (:op instruction)
        x (:x instruction)
        y (:y instruction)
        y-val (if (keyword? y)
                (get registers y)
                y)
        result
        (cond

          (= :set op)
          (let [updated-registers (assoc registers x y-val)]
            {:instruction instruction :registers updated-registers})

          (= :sub op)
          (let [updated-value (- (get registers x) y-val)
                updated-registers (assoc registers x updated-value)]
            {:instruction instruction :registers updated-registers})

          (= :mul op)
          (let [updated-value (* (get registers x) y-val)
                updated-registers (assoc registers x updated-value)]
            {:instruction instruction :registers updated-registers})

          (= :jnz op)
          (let [x-val (if (keyword? x)
                        (get registers x)
                        x)
                index-offset (if (not= x-val 0)
                               (if (keyword? y)
                                 (get registers y)
                                 y)
                               0)]
            {:instruction instruction :registers registers :index-offset index-offset})

          )]
    (assoc result
      :instruction instruction))
  )


(defn instruction-in-range?
  [instructions index]
  (contains? (set (range (count instructions))) index))


(defn follow-instructions
  [registers instructions]
  (loop [history []
         registers registers
         instruction-index 0]
    (if (not (instruction-in-range? instructions instruction-index))
      history
      (let [instruction (get instructions instruction-index)
            result (perform-instruction registers instruction)
            index-offset (:index-offset result 1)
            updated-offset (if (zero? index-offset) 1 index-offset)
            updated-registers (:registers result)
            updated-index (+ instruction-index updated-offset)]
        (recur (conj history result)
               updated-registers
               updated-index)))))


(defn part1
  []
  (count
    (filter #(= :mul %)
            (map #(get-in % [:instruction :op])
                 (follow-instructions registers instructions)))))


;
; Part 2
;


(defn follow-instructions2
  [n registers instructions]
  (loop [countdown n
         indexes-seen #{}
         registers registers
         instruction-index 0]
    (if (or (not (instruction-in-range? instructions instruction-index))
            (= countdown 0))
      {:indexes-seen (sort indexes-seen) :registers registers}
      (let [instruction (get instructions instruction-index)
            result (assoc (perform-instruction registers instruction) :instruction-index instruction-index)
            index-offset (:index-offset result 1)
            updated-offset (if (zero? index-offset) 1 index-offset)
            updated-registers (:registers result)
            updated-index (+ instruction-index updated-offset)]
        ;(println "instruction: " instruction " (index: " instruction-index " )")
        ;(println "updated registers: " updated-registers)
        (recur (dec countdown)
               (conj indexes-seen instruction-index)
               updated-registers
               updated-index)))))


(comment
  (follow-instructions2 n (assoc-in registers [:a] 1) instructions))


(defn init
  "Sets:
  - b
  - c"
  [registers]

  (let [b (- (* 57 100) 100000)
        c (- b 17000)]
    (assoc registers :b b :c c)))


(defn loop-from-instruction-19
  "Modifies:
  - d
  - e
  - f
  - g"
  [registers]

  (loop [registers registers
         g (:g registers)]
    (let [d (:e registers)
          b (:b registers)
          e (:e registers)
          g1 (- (* d e) b)
          f (if (zero? g)
              0
              (:f registers))
          e1 (dec e)
          g2 (- e1 b)]
      (if (zero? g2)
        registers
        (recur (assoc registers :d d :e e1 :f f :g g1)
               g2)))))

(defn loop-from-instruction-23
  "Modifies:
  - d
  - e
  - g"
  [registers]

  (loop [registers registers
         g (:g registers)]
    (let [e 2
          update1 (assoc registers :e e)
          update2 (loop-from-instruction-19 update1)
          d (dec (:d update2))
          b (:b update2)
          g1 (- d b)]
      (if (zero? g1)
        registers
        (recur (assoc update2 :d d :g g1)
               g1)))))



(defn loop-from-instruction-31
  "Modifies:
   - b
   - d
   - f
   - g
   - (conditionally based on f) h."
  [registers]

  (loop [registers registers
         g (:g registers)]

    (let [f 1
          d 2
          updated1 (assoc registers :f f :d d)
          updated2 (loop-from-instruction-23 updated1)
          f1 (:f updated2)
          h (if (zero? f1)
              (dec (:h registers))
              (:h registers))
          b (:b updated2)
          g1 (- b (:c registers))
          b1 (if (zero? b)
               (dec b)
               b)]
      (if (zero? g1)
        registers
        (recur (assoc updated2 :b b1 :f f1 :g g1 :h h)
               g1)))))


(defn part2
  [registers]

  (loop-from-instruction-31 (init registers)))
