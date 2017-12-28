(ns advent-of-code-2017.day10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day10 :refer :all]))

(def sample-nums [0, 1, 2, 3, 4])

(def sample-lengths [3, 4, 1, 5])

(deftest sample-test
  (testing "Tests knot-hash using sample inputs"
    (is (=  12 (knot-hash sample-nums sample-lengths)))))

