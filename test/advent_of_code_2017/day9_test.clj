(ns advent-of-code-2017.day9-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day9 :refer :all]))

(def day9-test-parser
  (create-parser))

(deftest group-containing-empty-garbage
  (testing "Empty garbage"
    (is (= [:S [:group [:garbage]]] (day9-test-parser "{<>}")))))

(deftest group-containing-garbage-lots-of-brackets
  (testing "Garbage with lots of brackets"
    (is (= [:S [:group [:garbage "<" "<" "<"]]] (day9-test-parser "{<<<<>}")))))

(deftest group-containing-garbage-bang-bracket
  (testing "Garbage with bang-bracket"
    (is (= [:S [:group [:garbage "{" "!" ">" "}"]]] (day9-test-parser "{<{!>}>}")))))

(deftest group-containing-garbage-double-bang
  (testing "Garbage with double-bang"
    (is (= [:S [:group [:garbage "!" "!"]]] (day9-test-parser "{<!!>}")))))

(deftest group-containing-garbage-mess
  (testing "Garbage with messy contents"
    (is (= [:S
            [:group
             [:garbage
              "{"
              "o"
              "\""
              "i"
              "!"
              "a"
              ","
              "<"
              "{"
              "i"
              "<"
              "a"]]] (day9-test-parser "{<{o\"i!a,<{i<a>}")))))

(deftest simple-group
  (testing "Single group"
    (is (= [:S [:group]] (day9-test-parser "{}")))))

(deftest nested-three-groups
  (testing "Nested three groups"
    (is (= [:S [:group [:group [:group]]]] (day9-test-parser "{{{}}}")))))

(deftest less-nested-three-groups
  (testing "Less nested three groups"
    (is (= [:S [:group [:group] [:group]]] (day9-test-parser "{{},{}}")))))

(deftest nested-six-groups
  (testing "Nested six groups"
    (is (= [:S [:group [:group [:group] [:group] [:group [:group]]]]] (day9-test-parser "{{{},{},{{}}}}")))))

(deftest single-group-garbage
  (testing "Single group with garbage"
    (is (= [:S
            [:group
             [:garbage
              "{"
              "}"
              ","
              "{"
              "}"
              ","
              "{"
              "{"
              "}"
              "}"]]] (day9-test-parser "{<{},{},{{}}>}")))))

(deftest single-group-a-garbage
  (testing "Single group with garbage containing 'a'"
    (is (= [:S
            [:group
             [:garbage "a"]
             [:garbage "a"]
             [:garbage "a"]
             [:garbage "a"]]] (day9-test-parser "{<a>,<a>,<a>,<a>}")))))

(deftest single-group-after-lots-of-bang-brackets
  (testing "Single group after lots of '!>' garbage"
    (is (= [:S
            [:group
             [:group
              [:garbage
               "!"
               ">"
               "}"
               ","
               "{"
               "<"
               "!"
               ">"
               "}"
               ","
               "{"
               "<"
               "!"
               ">"
               "}"
               ","
               "{"
               "<"
               "a"]]]] (day9-test-parser "{{<!>},{<!>},{<!>},{<a>}}")))))


(deftest score-single-group
  (testing "Score single group"
    (is (= 1 (score-groups (day9-test-parser "{}"))))))

(deftest score-triple-nested-group
  (testing "Score triple-nested group"
    (is (= 6 (score-groups (day9-test-parser "{{{}}}"))))))

(deftest score-nested-pair-group
  (testing "Score nested-pair group"
    (is (= 5 (score-groups (day9-test-parser "{{},{}}"))))))

(deftest score-sixteen-nested-group
  (testing "Score sixteen-nested group"
    (is (= 16 (score-groups (day9-test-parser "{{{},{},{{}}}}"))))))

(deftest score-group-with-garbage
  (testing "Score group with garbage"
    (is (= 1 (score-groups (day9-test-parser "{<a>,<a>,<a>,<a>}"))))))

(deftest score-nested-groups-with-garbage
  (testing "Score nested groups with garbage"
    (is (= 9 (score-groups (day9-test-parser "{{<ab>},{<ab>},{<ab>},{<ab>}}"))))))

(deftest score-nested-groups-with-cancels
  (testing "Score nested groups with garbage with cancels"
    (is (= 9 (score-groups (day9-test-parser "{{<!!>},{<!!>},{<!!>},{<!!>}}"))))))

(deftest score-nested-groups-with-cancel-with-char
  (testing "Score nested groups with garbage with cancel/char"
    (is (= 3 (score-groups (day9-test-parser "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))))




