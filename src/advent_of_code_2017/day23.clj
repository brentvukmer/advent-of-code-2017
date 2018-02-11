(ns advent-of-code-2017.day23
  (:require [advent-of-code-2017.day18 :as duet]
            [clojure.math.combinatorics :as combo]))


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

  (let [a 1
        b (- (* 57 100) -100000)
        c (- b -17000)]
    (assoc registers :a a :b b :c c)))


(def part2-initial-registers (init registers))


(defn loop19
  "Modifies:
  - d
  - e
  - f
  - g"
  [registers]

  (loop [registers registers]
    (let [d (:d registers)
          b (:b registers)
          e (:e registers)                                  ;; e starts loop at 1
          g1 (- (* d e) b)
          f (if (zero? g1)
              0
              (:f registers))
          e1 (inc e)
          g2 (- e1 b)
          updated-registers (assoc registers :d d :e e1 :f f)]
      (if (zero? g2)
        updated-registers
        (recur (assoc updated-registers :g g2))))))


(defn loop23
  "Modifies:
  - d
  - e
  - g"
  [registers]

  (loop [registers registers]
    (let [e 2
          updated-registers (loop19 (assoc registers :e e))
          d (inc (:d updated-registers))                    ;; d starts loop at 1
          g (- d (:b updated-registers))]
      (if (zero? g)
        (assoc updated-registers :d d :g g)
        (recur (assoc updated-registers :d d :g g))))))


(defn loop31
  "Initializes:
  - f to 1
  - d to 2

  Updates:
   - g (using b and c)
   - (conditionally based on f) h."
  [registers]

  (loop [registers registers]

    (let [updated-registers (loop23 (assoc registers :f 1 :d 2))
          h (if (zero? (:f updated-registers))
              (inc (:h updated-registers))
              (:h registers))
          b (:b updated-registers)
          g (- b (:c part2-initial-registers))]
      (if (zero? g)
        (assoc updated-registers :g g :h h)
        (recur (assoc updated-registers :b (+ b 17) :g g :h h)))))) ;; I don't think the updated g value is used


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    Instructions 0-7 only done once
;
;0   set b 57
;1   set c b ;; Redundant w/ instruction 6
;2   jnz a 2 ;; 'a' is always 1, so 'jnz a 2' is equivalent to 'jnz 1 2'
;3   jnz 1 5 ;; Never executed
;4   mul b 100
;5   sub b -100000
;6   set c b
;7   sub c -17000
;                 c doesn't change after this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    Loop from instruction 31
;
;8   set f 1
;9   set d 2
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    Loop from instruction 23
;
;10  set e 2
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    Loop from instruction 19
;
;11  set g d
;12  mul g e
;13  sub g b ;; Zero when d*e equals b
;14  jnz g 2
;;;; Only when g is zero ;;;;
;15  set f 0
;16  sub e -1 ;; Equivalent to 'add e 1'
;17  set g e
;18  sub g b ;; Zero when e+1 equals b
;
;;;; Loop until 'g' is zero ;;;;
;19  jnz g -8 ;; Fixed distance (always jump to instruction 11)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;20  sub d -1 ;; Equivalent to 'add d 1'
;21  set g d
;22  sub g b ;; Zero when d+1 equals b
;
;;;; Loop until g is zero ;;;;
;23  jnz g -13 ;; Fixed distance (always jump to instruction 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;24  jnz f 2
;
;;;; Only when f is zero ;;;;
;25  sub h -1 ;; Equivalent to 'add h 1'
;; This is the only register checked at the end
;
;26  set g b
;27  sub g c
;
;28  jnz g 2 ;; Zero when b equals c
;               We know that c does not change after init.
;               We know that at the start (after init) that c is 17000 less than b.
;               Since we are incrementing b by 17, loop31 will run 1000 times.
;
;;;; Only when g is zero
;29  jnz 1 3 ;; EXIT PROGRAM
;
;30  sub b -17 ;; Equivalent to 'add b 17'
;31  jnz 1 -23 ;; Fixed distance (always jump to instruction 8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; loop31 exits (and the program exits) when g is zero.
; loop23 and loop19 do not modify b.
;
; h gets incremented in loop31 if f is zero.
; Q: How many times does h get incremented?  (If we know this, we're done.)
; A:
;
; Q: How many times does loop31 run?
; A: loop31 will run 1000 times. (See notes above for instruction 28.)
;
; Q: How many times does loop23 run per loop31 run?
; A: 114189500 (see code below)


(defn loop-run-counts
  []
  (let [b-vals (range 105700 122700 17)
        d-e-vals (map #(+ % -2) b-vals)
        d-e-count (reduce + d-e-vals)]
    {:loop31 (/ 17000 17)
     :loop23 d-e-count
     :loop19 (* d-e-count d-e-count)}))

;
; Q: How many times does loop19 run per loop23 run?
; A: 114189500 (same logic as for counting loop23 runs per loop31 run)
;
; Q: How many times in each set of loop19's runs does f get set to zero?
; A:
;

(defn matching-factors
  [n]
  (let [factors (range 2 n)
        valid-factors (filter #(zero? (mod n %)) factors)
        combos (combo/combinations valid-factors 2)
        matching (filter #(= n (* (first %) (second %))) combos)]
    matching))


(defn find-f0-vals
  []
  (sort
    (into {}
          (for [b (range 105700 122717 17)
                :let [matching (matching-factors b)]
                :when (not (empty? matching))]
            [b matching]))))


(defn part2
  []
  (count (find-f0-vals)))
