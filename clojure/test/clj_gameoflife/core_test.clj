(ns clj-gameoflife.core-test
  (:require [clojure.test :refer :all]
            [clj-gameoflife.core :refer :all]))

(deftest test-create-empty-world
  (testing "Create an empty square world"
    (let [empty-5-5 (create-empty-world 5 5)]
      (is (= 5 (count empty-5-5)))
      (is (= 5 (count (first empty-5-5))))))
  (testing "Create an empty rectangular world"
    (let [empty-4-5 (create-empty-world 4 5)]
      (is (= 5 (count empty-4-5)))
      (is (= 4 (count (first empty-4-5)))))))

(deftest test-create-world
  (testing "Create a square world"
    (let [grid-5-5 (create-world 5 5 #{[2 2] [3 2] [1 0]})]
      (is (= "X" ((grid-5-5 2) 2)))
      (is (= "X" ((grid-5-5 3) 2)))
      (is (= "X" ((grid-5-5 1) 0)))
      (is (= "." ((grid-5-5 2) 0)))
      (is (= "." ((grid-5-5 2) 3)))))
  (testing "Create a rectangular world"
    (let [grid-5-5 (create-world 5 4 #{[2 2] [3 2] [4 3]})]
      (is (= "X" ((grid-5-5 2) 2)))
      (is (= "X" ((grid-5-5 3) 2)))
      (is (= "X" ((grid-5-5 4) 3)))
      (is (= "." ((grid-5-5 2) 0)))
      (is (= "." ((grid-5-5 2) 3))))))

(deftest test-neighbours
  (testing "Calculating neighbours"
    (let [n (set (neighbours [2 2]))]
      (is (= true (contains? n [1 1])))
      (is (= true (contains? n [1 2])))
      (is (= true (contains? n [1 3])))
      (is (= true (contains? n [2 1])))
      (is (= true (contains? n [2 3])))
      (is (= true (contains? n [3 1])))
      (is (= true (contains? n [3 2])))
      (is (= true (contains? n [3 3])))
      (is (= false (contains? n [2 2]))))
    (let [n (set (neighbours [0 0]))]
      (is (= false (contains? n [-1 -1])))
      (is (= false (contains? n [0 -1])))
      (is (= true (contains? n [1 0])))
      (is (= true (contains? n [0 1])))
      (is (= true (contains? n [1 1])))
      (is (= false (contains? n [0 0]))))))

(deftest test-step
  (testing "Generating a step"
    (let [living-cells glider
          next-step (step living-cells)]
      (is (= true (contains? next-step [2 2])))
      (is (= true (contains? next-step [1 0])))
      (is (= true (contains? next-step [3 1])))
      (is (= true (contains? next-step [2 1])))
      (is (= true (contains? next-step [1 2])))
      (is (= false (contains? next-step [4 1]))))))
