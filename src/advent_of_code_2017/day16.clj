(ns advent-of-code-2017.day16
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.edn :as edn]))

;
; Part 1
;

(def input
  (map str/trim-newline (str/split (slurp (io/resource "day16")) #",")))

(def program-line (mapv char (range (int \a) (int \q))))

(def sample-line (vec "abcde"))

(defn exchange
  ""
  [move-str line]
  (let [positions-str (apply str (rest move-str))
        positions-input (str/replace (str "[" positions-str "]") #"\/" ",")
        [a b] (edn/read-string positions-input)
        v (vec line)
        xa (assoc-in v [a] (get v b))
        xb (assoc-in xa [b] (get v a))]
    xb))

(defn swap
  ""
  [move-str line]
  (let [positions-str (apply str (rest move-str))
        num-programs (edn/read-string positions-str)
        split-count (- (count line) num-programs)]
    (apply concat (reverse (split-at split-count line))))
  )

(defn partner
  ""
  [move-str line]
  (let [programs-str (apply str (rest move-str))
        programs-input (str/replace (str "[" programs-str "]") #"\/" ",")
        programs (edn/read-string programs-input)
        [p1 p2] (filter some? (map-indexed (fn [idx x]
                                             (let [sym (symbol (str x))]
                                               (when (or (= (first programs) sym)
                                                         (= (second programs) sym))
                                                 [idx x]))) line))
        v (vec line)
        x1 (assoc-in v [(first p1)] (get v (first p2)))
        x2 (assoc-in x1 [(first p2)] (get v (first p1)))]
    x2))

(defn do-move
  ""
  [line move-str]
  (let [move-code (first move-str)]
    (cond
      (= \x move-code)
      (exchange move-str line)

      (= \s move-code)
      (swap move-str line)

      (= \p move-code)
      (partner move-str line)

      :else
      :no-such-move-error)))

(defn do-moves
  ""
  [moves line]
  (reduce do-move line moves))