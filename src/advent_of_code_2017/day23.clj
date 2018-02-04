(ns advent-of-code-2017.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code-2017.day18 :as duet]))


;
; Part 1
;


(def instructions (mapv duet/parse-instruction (duet/read-inputs "day23")))


(def registers (into {}
                     (mapv
                       #(vector (keyword (str (char %))) 0)
                       (range (int \a) (int \i)))))


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
          )]
    (assoc result
      :instruction instruction
      :time (. System currentTimeMillis)
      :thread (.getName (Thread/currentThread))))
  )

;
; Part 2
;


