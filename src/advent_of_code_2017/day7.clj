(ns advent-of-code-2017.day7
  (:require [clojure.java.io :as io]))

;
; Part 1
;

(defn parse-program-record
  ""
  [line]
  (let [tokens (vec (remove #(= "->" %) (clojure.string/split line #"\s")))]
    {(keyword (clojure.string/trim (first tokens)))
     {:weight   (first (clojure.edn/read-string (second tokens)))
      :programs (map (comp keyword #(clojure.string/replace % "," "")) (drop 2 tokens))
      }}))

(defn read-lines-classpath-resource
  ""
  [name]
  (map #(parse-program-record %) (clojure.string/split-lines (slurp (io/resource name)))))

(def day7-inputs
  (into {} (read-lines-classpath-resource "day7")))

(def day7-test-inputs
  (into {} (map #(parse-program-record %) (clojure.string/split-lines "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"))))

(defn find-root-program
  "docstring"
  [program-records]
  (let [parents (set (keys (into {} (remove #(empty? (:programs (val %))) program-records))))
        children (into #{} (mapcat #(:programs (% program-records)) parents))]
    (first (clojure.set/difference parents children))))
;
; Part 2
;

