(ns sudoku-solver.core-test
  (:require [clojure.test :refer :all]
            [sudoku-solver.core :refer :all]))

(deftest parse-puzzle-file-test
  (testing "parse-puzzle-file"
    (is (=
         [[nil nil \8 nil nil nil nil \3 \1]
          [nil nil nil nil \6 nil \2 \7 nil]
          [\7 nil nil \1 \8 \2 nil \6 nil]
          [\9 nil nil \5 \3 \4 \1 nil nil]
          [nil nil \5 \6 nil \8 \7 nil nil]
          [nil nil \6 \7 \2 \1 nil nil \5]
          [nil \3 nil \2 \1 \9 nil nil \7]
          [nil \8 \2 nil \4 nil nil nil nil]
          [\1 \7 nil nil nil nil \4 nil nil]]
         (parse-puzzle-file "resources/easy")))))

(deftest get-quadrant-test
  (testing "get-quadrant in first quadrant"
    (is (= {\1 nil
            \2 '(1 2)
            \3 '(2 0)
            \4 nil
            \5 nil
            \6 '(0 1)
            \7 nil
            \8 nil
            \9 '(2 2)}
           (get-quadrant [[nil \6 nil nil nil \5 nil \1 \9]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [\3 nil \9 \8 \1 nil nil nil \8]] '(0 0)))))
  (testing "get-quadrant in second quadrant"
    (is (= {\1 '(1 2)
            \2 '(2 1)
            \3 '(1 0)
            \4 nil
            \5 '(0 2)
            \6 nil
            \7 nil
            \8 '(2 0)
            \9 nil}
           (get-quadrant [[nil \6 nil nil nil \5 nil \1 \9]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [\3 nil \9 \8 \2 nil nil nil \8]] '(0 1)))))
  (testing "get-quadrant in eighth quadrant"
    (is (= {\1 nil
            \2 '(1 1)
            \3 '(2 2)
            \4 nil
            \5 '(0 0)
            \6 nil
            \7 nil
            \8 '(2 0)
            \9 '(0 1)}
           (get-quadrant [[nil \6 nil nil nil \5 nil \1 \9]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil nil \5 \9 nil nil nil nil]
                          [nil nil nil nil \2 nil nil nil nil]
                          [nil nil nil \8 nil \3 nil nil nil]] '(2 1))))))

(deftest assign-number-in-quadrant-test
  (testing "assign number 1 in the first quadrant"
    (is (= [[nil nil nil nil nil \1  nil nil nil]
            [nil nil \1  nil nil nil nil nil nil]
            [nil nil nil nil nil nil \1  nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil \1  nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [\1  nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-quadrant '(0 0) \1 [[nil nil nil nil nil \1  nil nil nil]
                                                [nil nil nil  nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil \1  nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]]))))

  (testing "does not assign number 1 in the first quadrant when not possible"
    (is (= [[nil nil nil nil nil \1  nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil \1  nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [\1  nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-quadrant '(0 0) \1 [[nil nil nil nil nil \1 nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]]))))
  (testing "assigns number 1 in the third quadrant"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil \1  nil nil nil]
            [\1  nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil \1  nil]
            [nil nil nil nil nil nil nil nil  \1]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-quadrant '(0 2) \1 [[nil nil nil nil nil nil \1 nil nil]
                                                [nil nil nil nil nil \1  nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil \1  nil]
                                                [nil nil nil nil nil nil nil nil  \1]
                                                [nil nil nil nil nil nil nil nil nil]])))))
  (deftest lateral-sibling-quadrants-test
    (testing "lateral-sibling-quadrants"
      (is (= #{'(0 1) '(0 2)} (lateral-sibling-quadrants '(0 0))))
      (is (= #{'(2 0) '(2 2)} (lateral-sibling-quadrants '(2 1))))))

  (deftest vertical-sibling-quadrants-test
    (testing "vertical-sibling-quadrants"
      (is (= #{'(1 0) '(2 0)} (vertical-sibling-quadrants '(0 0))))
      (is (= #{'(1 1) '(0 1)} (vertical-sibling-quadrants '(2 1))))))

  (deftest sibling-eliminated-coordinates-test
    (testing "impossible-coordinates-for-quadrant"
      (is (= #{'(0 0) '(0 1) '(0 2) '(2 0) '(2 1) '(2 2) '(1 0) '(1 1)}
             (sibling-eliminated-coordinates [[nil nil nil nil nil \1  nil nil nil]
                                                [nil nil \1  nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil \1  nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] '(0 0) \1)))))

