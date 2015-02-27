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
