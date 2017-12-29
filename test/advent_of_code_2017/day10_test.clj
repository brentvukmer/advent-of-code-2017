(ns advent-of-code-2017.day10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day10 :refer :all]))

(def sample-nums [0, 1, 2, 3, 4])

(def sample-lengths [3, 4, 1, 5])

(deftest sample-test
  (testing "Tests knot-hash using sample inputs"
    (is (= 12 (knots-hash (do-knots sample-nums sample-lengths))))))

(deftest sample-round1
  (testing "Testing round 1 of knot-hashing the sample inputs")
  (is (= [2 1 0 3 4] (knot sample-nums 0 3))))

(deftest sample-round2
  (testing "Testing round 2 of knot-hashing the sample inputs"
    (is (= [4 3 0 1 2] (knot [2 1 0 3 4] 3 4)))))

(deftest sample-round3
  (testing "Testing round 3 of knot-hashing the sample inputs"
    (is (= [4 3 0 1 2] (knot [4 3 0 1 2] 3 1)))))

(deftest sample-round4
  (testing "Testing round 4 of knot-hashing the sample inputs"
    (is (= [3 4 2 1 0] (knot [4 3 0 1 2] 1 5)))))


