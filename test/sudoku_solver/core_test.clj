(ns sudoku-solver.core-test
  (:require [clojure.test :refer :all]
            [sudoku-solver.core :refer :all]))

(deftest parse-puzzle-file-test
  (testing "parse-easy-puzzle-file"
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
         (parse-puzzle-file "resources/easy"))))
  (testing "parse-medium-puzzle-file"
    (is (=
         [[nil \9  nil nil nil \6  nil \5  nil]
          [nil nil nil nil \1  nil \3  \8  nil]
          [nil \8  nil nil \3  nil nil nil \6 ]
          [\1  \3  \9  nil nil nil \4  \6  nil ]
          [nil nil nil \6  nil \3  nil nil nil]
          [nil \6  \2  nil nil nil \7  \3  \5 ]
          [\6  nil nil nil \8  nil nil \7  nil]
          [nil \7  \8  nil \6  nil nil nil nil]
          [nil \5  nil \3  nil nil nil \9  nil]]
         (parse-puzzle-file "resources/medium")))))

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
                                                 [nil nil nil nil \3  nil nil nil nil]]))))

  (testing "assigns number 9 in the fifth quadrant by realizing any other assignment would prevent assignment in the second and eighth quadrant"
    (is (= [[\3  \9  nil \8  nil \6  nil \5  nil]
            [nil nil \6  nil \1  nil \3  \8  nil]
            [nil \8  nil nil \3  nil \9  nil \6]
            [\1  \3  \9  nil nil \8  \4  \6  \2]
            [nil nil nil \6  \2  \3  \8  \1  \9]
            [\8  \6  \2  nil \9  nil \7  \3  \5]
            [\6  nil \3  nil \8  nil nil \7  nil]
            [\9  \7  \8  nil \6  nil nil nil \3]
            [nil \5  nil \3  nil nil \6  \9  \8]]
           (assign-number-in-quadrant '(1 1) \9 [[\3  \9  nil \8  nil \6  nil \5  nil]
                                                 [nil nil \6  nil \1  nil \3  \8  nil]
                                                 [nil \8  nil nil \3  nil \9  nil \6]
                                                 [\1  \3  \9  nil nil \8  \4  \6  \2]
                                                 [nil nil nil \6  \2  \3  \8  \1  \9]
                                                 [\8  \6  \2  nil nil nil \7  \3  \5]
                                                 [\6  nil \3  nil \8  nil nil \7  nil]
                                                 [\9  \7  \8  nil \6  nil nil nil \3]
                                                 [nil \5  nil \3  nil nil \6  \9  \8]]))))

  (testing "assigns number 2 in the third quadrant by realizing any other assignment would prevent assignment in the second and eighth quadrant"
    (is (= [[\3  \9  nil \8  nil \6  \2  \5  nil]
            [nil nil \6  nil \1  nil \3  \8  nil]
            [nil \8  nil nil \3  nil \9  nil \6]
            [\1  \3  \9  \7  \5  \8  \4  \6  \2]
            [nil nil nil \6  \2  \3  \8  \1  \9]
            [\8  \6  \2  nil \9  nil \7  \3  \5]
            [\6  nil \3  nil \8  nil nil \7  nil]
            [\9  \7  \8  nil \6  nil nil nil \3]
            [nil \5  nil \3  nil nil \6  \9  \8]]
           (assign-number-in-quadrant '(0 2) \2 [[\3  \9  nil \8  nil \6  nil \5  nil]
                                                 [nil nil \6  nil \1  nil \3  \8  nil]
                                                 [nil \8  nil nil \3  nil \9  nil \6]
                                                 [\1  \3  \9  \7  \5  \8  \4  \6  \2]
                                                 [nil nil nil \6  \2  \3  \8  \1  \9]
                                                 [\8  \6  \2  nil \9  nil \7  \3  \5]
                                                 [\6  nil \3  nil \8  nil nil \7  nil]
                                                 [\9  \7  \8  nil \6  nil nil nil \3]
                                                 [nil \5  nil \3  nil nil \6  \9  \8]])))))

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

  (deftest solve-puzzle-test
    (testing "solve-puzzle solves the easy puzzle"
      (is (= [[\2  \6  \8  \4  \7  \5  \9  \3  \1]
              [\4  \5  \1  \9  \6  \3  \2  \7  \8]
              [\7  \9  \3  \1  \8  \2  \5  \6  \4]
              [\9  \2  \7  \5  \3  \4  \1  \8  \6]
              [\3  \1  \5  \6  \9  \8  \7  \4  \2]
              [\8  \4  \6  \7  \2  \1  \3  \9  \5]
              [\6  \3  \4  \2  \1  \9  \8  \5  \7]
              [\5  \8  \2  \3  \4  \7  \6  \1  \9]
              [\1  \7  \9  \8  \5  \6  \4  \2  \3]]

(solve-puzzle [[nil nil \8  nil nil nil nil \3  \1]
              [nil nil nil nil \6  nil \2  \7  nil]
              [\7  nil nil \1  \8  \2  nil \6  nil]
              [\9  nil nil \5  \3  \4  \1  nil nil]
              [nil nil \5  \6  nil \8  \7  nil nil]
              [nil nil \6  \7  \2  \1  nil nil \5 ]
              [nil \3  nil \2  \1  \9  nil nil \7 ]
              [nil \8  \2  nil \4  nil nil nil nil]
              [\1  \7  nil nil nil nil \4  nil nil]]))))
    (testing "solve-puzzle throws an exception if it is unable to solve the puzzle"
      (is (thrown? Exception
                   (solve-puzzle [[nil nil nil nil nil nil nil nil nil]
                                  [\5  nil nil nil nil nil nil nil nil]
                                  [\2  nil nil nil nil nil nil nil nil]
                                  [nil \7  \2  nil nil nil nil nil nil]
                                  [\4  \6  \5  nil nil nil nil nil nil]
                                  [\8  \1  \9  nil nil nil nil nil nil]
                                  [nil \5  \8  nil nil nil nil nil nil]
                                  [\1  \4  \3  nil nil nil nil nil nil]
                                  [\7  \2  \6  nil nil nil nil nil nil]])))))

  (deftest display-puzzle-test
    (testing "display-puzzle displays the puzzles in a readable form"
      (is (= "| - - - - - - - - - - - |\n| 2 6 8 | 4 7 5 | 9 3 1 |\n| 4 5 1 | 9 6 3 | 2 7 8 |\n| 7 9 3 | 1 8 2 | 5 6 4 |\n| - - - | - - - | - - - |\n| 9 2 7 | 5 3 4 | 1 8 6 |\n| 3 1 5 | 6 9 8 | 7 4 2 |\n| 8 4 6 | 7 2 1 | 3 9 5 |\n| - - - | - - - | - - - |\n| 6 3 4 | 2 1 9 | 8 5 7 |\n| 5 8 2 | 3 4 7 | 6 1 9 |\n| 1 7 9 | 8 5 6 | 4 2 3 |\n| - - - - - - - - - - - |"
             (display-puzzle [[\2  \6  \8  \4  \7  \5  \9  \3  \1]
                              [\4  \5  \1  \9  \6  \3  \2  \7  \8]
                              [\7  \9  \3  \1  \8  \2  \5  \6  \4]
                              [\9  \2  \7  \5  \3  \4  \1  \8  \6]
                              [\3  \1  \5  \6  \9  \8  \7  \4  \2]
                              [\8  \4  \6  \7  \2  \1  \3  \9  \5]
                              [\6  \3  \4  \2  \1  \9  \8  \5  \7]
                              [\5  \8  \2  \3  \4  \7  \6  \1  \9]
                              [\1  \7  \9  \8  \5  \6  \4  \2  \3]])))))

  (deftest display-puzzle-row-test
    (testing "display-puzzle-row displays the puzzle row"
      (is (= "| 1 2 3 | 4 5 6 | 7 8 9 |"
             (display-puzzle-row [1 2 3 4 5 6 7 8 9])))))

