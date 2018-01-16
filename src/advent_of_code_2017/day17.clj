(ns advent-of-code-2017.day17)

;
; Part 1
;

(def sample-step-count 3)

(def step-count 382)

(def initial-buffer '(0))

(def values (range 1 2018))

(defn populate-buffer
  ""
  [num-steps buffer vals]
  (reduce
    (fn [[buf idx] val]
      (let [stepped-idx (mod (+ idx num-steps) (count buf))
            next-idx (inc stepped-idx)
            [buf1 buf2] (split-at next-idx buf)
            updated-buf (concat buf1 (list val) buf2)]
        [updated-buf next-idx]
        ))
    [buffer 0]
    vals))

(defn value-after-last-written
  ""
  [num-steps buffer vals]
  (let [[final-buf last-val-idx] (populate-buffer num-steps buffer vals)]
    (nth final-buf (inc last-val-idx))))

;(value-after-last-written sample-step-count initial-buffer values)
;=> 638
;(value-after-last-written step-count initial-buffer values)
;=> 1561

;
; Part 2
;