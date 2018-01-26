(ns advent-of-code-2017.day20
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;
; Part 1
;


(defn read-input
  ""
  [input-name]
  (clojure.string/split-lines (slurp (io/resource input-name))))

(defn parse-input-line
  ""
  [input-line]
  (let [cleaned-up
        (-> (str/replace input-line "=" " ")
            (str/replace "<" "[")
            (str/replace ">" "]")
            (str/replace "p" ":p")
            (str/replace "v" ":v")
            (str/replace "a" ":a"))
        edn-str (str "{" cleaned-up "}")
        xyz (edn/read-string edn-str)]
     xyz))

;
; Part 2
;

