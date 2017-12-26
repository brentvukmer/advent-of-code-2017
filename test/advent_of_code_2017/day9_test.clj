(ns advent-of-code-2017.day9-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day9 :refer :all]
            [instaparse.core :as insta]))

(deftest group-containing-empty-garbage
  (testing "Empty garbage"
    (is (= [:S [:group [:garbage]]] (insta/parse day9-parser "{<>}")))))

(deftest group-containing-garbage-lots-of-brackets
  (testing "Garbage with lots of brackets"
    (is (= [:S [:group [:garbage [:garbage-item "<"] [:garbage-item "<"] [:garbage-item "<"]]]] (insta/parse day9-parser "{<<<<>}")))))

(deftest group-containing-garbage-bang-bracket
  (testing "Garbage with bang-bracket"
    (is (= [:S [:group [:garbage [:garbage-item "{"] [:garbage-item "!>"] [:garbage-item "}"]]]] (insta/parse day9-parser "{<{!>}>}")))))

(deftest group-containing-garbage-double-bang
  (testing "Garbage with double-bang"
    (is (= [:S [:group [:garbage [:garbage-item "!!"]]]] (insta/parse day9-parser "{<!!>}")))))

(deftest group-containing-garbage-mess
  (testing "Garbage with messy contents"
    (is (= [:S
            [:group
             [:garbage
              [:garbage-item "{"]
              [:garbage-item "o"]
              [:garbage-item "\""]
              [:garbage-item "i"]
              [:garbage-item "!"]
              [:garbage-item "a"]
              [:garbage-item ","]
              [:garbage-item "<"]
              [:garbage-item "{"]
              [:garbage-item "i"]
              [:garbage-item "<"]
              [:garbage-item "a"]]]] (insta/parse day9-parser "{<{o\"i!a,<{i<a>}")))))

(deftest simple-group
  (testing "Single group"
    (is (= [:S [:group]] (insta/parse day9-parser "{}")))))

(deftest nested-three-groups
  (testing "Nested three groups"
    (is (= [:S [:group [:group [:group]]]] (insta/parse day9-parser "{{{}}}")))))

(deftest less-nested-three-groups
  (testing "Less nested three groups"
    (is (= [:S [:group [:group] [:group]]] (insta/parse day9-parser "{{},{}}")))))

(deftest nested-six-groups
  (testing "Nested six groups"
    (is (= [:S [:group [:group [:group] [:group] [:group [:group]]]]] (insta/parse day9-parser "{{{},{},{{}}}}")))))

(deftest single-group-garbage
  (testing "Single group with garbage"
    (is (= [:S
            [:group
             [:garbage
              [:garbage-item "{"]
              [:garbage-item "}"]
              [:garbage-item ","]
              [:garbage-item "{"]
              [:garbage-item "}"]
              [:garbage-item ","]
              [:garbage-item "{"]
              [:garbage-item "{"]
              [:garbage-item "}"]
              [:garbage-item "}"]]]] (insta/parse day9-parser "{<{},{},{{}}>}")))))

(deftest single-group-a-garbage
  (testing "Single group with garbage containing 'a'"
    (is (= [:S
            [:group
             [:garbage [:garbage-item "a"]]
             [:garbage [:garbage-item "a"]]
             [:garbage [:garbage-item "a"]]
             [:garbage [:garbage-item "a"]]]] (insta/parse day9-parser "{<a>,<a>,<a>,<a>}")))))

(deftest single-group-after-lots-of-bang-brackets
  (testing "Single group after lots of '!>' garbage"
    (is (= [:S
            [:group
             [:group
              [:garbage
               [:garbage-item "!>"]
               [:garbage-item "}"]
               [:garbage-item ","]
               [:garbage-item "{"]
               [:garbage-item "<"]
               [:garbage-item "!>"]
               [:garbage-item "}"]
               [:garbage-item ","]
               [:garbage-item "{"]
               [:garbage-item "<"]
               [:garbage-item "!>"]
               [:garbage-item "}"]
               [:garbage-item ","]
               [:garbage-item "{"]
               [:garbage-item "<"]
               [:garbage-item "a"]]]]] (insta/parse day9-parser "{{<!>},{<!>},{<!>},{<a>}}")))))





