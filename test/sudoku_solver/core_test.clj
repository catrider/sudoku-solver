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
    (is (= [[nil nil nil nil nil nil \1 nil nil]
            [nil nil nil nil nil \1  nil nil nil]
            [\1  nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil \1 nil]
            [nil nil nil nil nil nil nil nil  nil]
            [nil nil nil nil nil nil nil nil  \1]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-quadrant '(0 2) \1 [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil \1  nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil \1 nil]
                                                [nil nil nil nil nil nil nil nil  nil]
                                                [nil nil nil nil nil nil nil nil  \1]
                                                [nil nil nil nil nil nil nil nil nil]]))))
  (testing "assigns a number in a quadrant when it is the last number to be assigned in that quadrant"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil \6  \4 \8  nil nil nil]
            [nil nil nil \1  \5  \3  nil nil nil]
            [nil nil nil \7  \9  \2  nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-quadrant '(1 1) \4 [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil \6  nil \8  nil nil nil]
                                                [nil nil nil \1  \5  \3  nil nil nil]
                                                [nil nil nil \7  \9  \2  nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]]))))
  (testing "does not assign a number in a quadrant if it is already in the quadrant"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil \6  nil \8  nil nil nil]
            [nil nil nil \1  \5  \3  nil nil nil]
            [nil nil nil \7  \9  \2  nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-quadrant '(1 1) \8 [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil \6  nil \8  nil nil nil]
                                                [nil nil nil \1  \5  \3  nil nil nil]
                                                [nil nil nil \7  \9  \2  nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]]))))
  (testing "assigns number 1 in the eighth quadrant"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil \1  nil nil nil]
            [nil nil nil \1  nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil \1  nil nil nil nil]
            [nil \1  nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil \1  nil nil]]
           (assign-number-in-quadrant '(2 1) \1 [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil \1  nil nil nil]
                                                [nil nil nil \1  nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil \1  nil nil]]))))

  (testing "assigns number 1 in the second quadrant when the 1 completes the row"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [\5  \6  \2  \8  \1  \4  \9  \3  \7 ]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-quadrant '(0 1) \1 [[nil nil nil nil nil nil nil nil nil]
                                                [\5  \6  \2  \8  nil  \4  \9  \3  \7]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]]))))

  (testing "assigns number 1 in the second quadrant when the 1 completes the column"
    (is (= [[nil nil nil nil \7  nil nil nil nil]
            [nil nil nil nil \1  nil nil nil nil]
            [nil nil nil nil \5  nil nil nil nil]
            [nil nil nil nil \2  nil nil nil nil]
            [nil nil nil nil \8  nil nil nil nil]
            [nil nil nil nil \9  nil nil nil nil]
            [nil nil nil nil \6  nil nil nil nil]
            [nil nil nil nil \4  nil nil nil nil]
            [nil nil nil nil \3  nil nil nil nil]]
           (assign-number-in-quadrant '(0 1) \1 [[nil nil nil nil \7  nil nil nil nil]
                                                 [nil nil nil nil nil nil nil nil nil]
                                                 [nil nil nil nil \5  nil nil nil nil]
                                                 [nil nil nil nil \2  nil nil nil nil]
                                                 [nil nil nil nil \8  nil nil nil nil]
                                                 [nil nil nil nil \9  nil nil nil nil]
                                                 [nil nil nil nil \6  nil nil nil nil]
                                                 [nil nil nil nil \4  nil nil nil nil]
                                                 [nil nil nil nil \3  nil nil nil nil]])))))

  (deftest lateral-sibling-quadrants-test
    (testing "lateral-sibling-quadrants"
      (is (= #{'(0 1) '(0 2)} (lateral-sibling-quadrants '(0 0))))
      (is (= #{'(0 0) '(0 1)} (lateral-sibling-quadrants '(0 2))))
      (is (= #{'(2 0) '(2 2)} (lateral-sibling-quadrants '(2 1))))))

  (deftest vertical-sibling-quadrants-test
    (testing "vertical-sibling-quadrants"
      (is (= #{'(1 0) '(2 0)} (vertical-sibling-quadrants '(0 0))))
      (is (= #{'(1 2) '(2 2)} (vertical-sibling-quadrants '(0 2))))
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

  (deftest number-at-coordinates-in-quadrant-completes-row?-test
    (testing "number-at-coordinates-in-quadrant-completes-row? returns true when the number completes the row"
      (is (= true (number-at-coordinates-in-quadrant-completes-row? [[nil nil nil nil nil nil nil nil nil]
                                                                               [nil nil nil nil nil nil nil nil nil]
                                                                               [nil nil nil nil nil nil nil nil nil]
                                                                               [nil nil nil nil nil nil nil nil nil]
                                                                               [nil nil nil nil nil nil nil nil nil]
                                                                               [nil nil nil nil nil nil nil nil nil]
                                                                               [nil nil nil nil nil nil nil nil nil]
                                                                               [\6  \3  \8  \1  \7  \2  \9  nil \5]
                                                                               [nil nil nil nil nil nil nil nil nil]] '(2 2) '(1 1) \4))))
    (testing "number-at-coordinates-in-quadrant-completes-row? returns false when the number does not complete the row"
      (is (= false (number-at-coordinates-in-quadrant-completes-row? [[nil nil nil nil nil nil nil nil nil]
                                                                     [nil nil nil nil nil nil nil nil nil]
                                                                     [nil nil nil nil nil nil nil nil nil]
                                                                     [nil nil nil nil nil nil nil nil nil]
                                                                     [nil nil nil nil nil nil nil nil nil]
                                                                     [nil nil nil nil nil nil nil nil nil]
                                                                     [nil nil nil nil nil nil nil nil nil]
                                                                     [\6  nil  \8  \1  \7  \2  \9  nil \5]
                                                                     [nil nil nil nil nil nil nil nil nil]] '(2 2) '(1 1) \4)))))

  (deftest number-at-coordinates-in-quadrant-completes-column?-test
    (testing "number-at-coordinates-in-quadrant-completes-column? returns true when the number completes the column"
      (is (= true (number-at-coordinates-in-quadrant-completes-column? [[nil nil \5  nil nil nil nil nil nil]
                                                                        [nil nil \8  nil nil nil nil nil nil]
                                                                        [nil nil \1  nil nil nil nil nil nil]
                                                                        [nil nil \7  nil nil nil nil nil nil]
                                                                        [nil nil nil nil nil nil nil nil nil]
                                                                        [nil nil \3  nil nil nil nil nil nil]
                                                                        [nil nil \9  nil nil nil nil nil nil]
                                                                        [nil nil \4  nil nil nil nil nil nil]
                                                                        [nil nil \6  nil nil nil nil nil nil]] '(1 0) '(1 2) \2))))

    (testing "number-at-coordinates-in-quadrant-completes-column? returns false when the number does not complete the column"
      (is (= false (number-at-coordinates-in-quadrant-completes-column? [[nil nil \5  nil nil nil nil nil nil]
                                                                         [nil nil \8  nil nil nil nil nil nil]
                                                                         [nil nil \1  nil nil nil nil nil nil]
                                                                         [nil nil \7  nil nil nil nil nil nil]
                                                                         [nil nil nil nil nil nil nil nil nil]
                                                                         [nil nil \3  nil nil nil nil nil nil]
                                                                         [nil nil \9  nil nil nil nil nil nil]
                                                                         [nil nil \4  nil nil nil nil nil nil]
                                                                         [nil nil nil nil nil nil nil nil nil]] '(2 2) '(1 1) \4)))))

