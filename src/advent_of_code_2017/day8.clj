(ns advent-of-code-2017.day8
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(defn read-lines-classpath-resource
  ""
  [name]
  (clojure.string/split-lines (slurp (io/resource name))))

(def day8-inputs
  (map #(clojure.string/split % #"\s") (read-lines-classpath-resource "day8")))

(def day8-test-inputs
  (map #(clojure.string/split % #"\s") (clojure.string/split-lines "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10")))

(defn initial-registers
  ""
  [inputs]
  (into {} (map vector (set (map (comp keyword first) inputs)) (repeat 0))))

;
; Convert input tokens into:
; - keyword
; - var
; - int
; - keyword
; - var
; - int
;

(defn token->arithmetic-operator
  ""
  [token]
  (cond
    (= token "inc")
    +
    (= token "dec")
    -
    :else
    (throw (IllegalArgumentException. (str "'" token "' is not 'inc' or 'dec'")))))

(defn token->comparison-operator
  ""
  [token]
  (cond
    (= token "==")
    =
    (= token "!=")
    (complement =)
    :else
    (find-var (symbol (str "clojure.core/" token)))))

(defn update-register
  ""
  ([registers input-tokens]
   (let [kw1 (keyword (first input-tokens))
         operator-var (token->arithmetic-operator (second input-tokens))
         delta (Integer/parseInt (get input-tokens 2))
         kw2 (keyword (get input-tokens 4))
         compare-fn-var (token->comparison-operator (get input-tokens 5))
         val (Integer/parseInt (get input-tokens 6))]
     (update-register registers kw1 operator-var delta kw2 compare-fn-var val)))
  ([registers kw1 operator-var delta kw2 compare-fn-var val]
   (if (compare-fn-var (kw2 registers) val)
     (assoc registers kw1 (operator-var (kw1 registers) delta))
     registers)))

(defn process-register-instructions
  ""
  [inputs]
  (loop [registers (initial-registers inputs)
         instructions inputs
         history []]
    (if (empty? instructions)
      history
      (recur (update-register registers (first instructions))
             (rest instructions)
             (conj history registers)))))

(defn max-register-val-after-updates
  ""
  [inputs]
  (apply max (vals (last (process-register-instructions inputs)))))

;
; Part 2
;

(defn max-register-val-in-update-history
  ""
  [inputs]
  (apply max (into #{} (mapcat vals (process-register-instructions inputs)))))