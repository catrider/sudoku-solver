(ns sudoku-solver.puzzle_test
  (:require [clojure.test :refer :all]
            [sudoku-solver.puzzle :refer :all]))

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
           (assign-number-in-quadrant          [[nil nil nil nil nil \1  nil nil nil]
                                                [nil nil nil  nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil \1  nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] '(0 0) \1))))

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
           (assign-number-in-quadrant          [[nil nil nil nil nil \1 nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] '(0 0) \1))))
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
           (assign-number-in-quadrant          [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil \1  nil nil nil]
                                                [\1  nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil \1 nil]
                                                [nil nil nil nil nil nil nil nil  nil]
                                                [nil nil nil nil nil nil nil nil  \1]
                                                [nil nil nil nil nil nil nil nil nil]] '(0 2) \1))))
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
           (assign-number-in-quadrant          [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil \6  nil \8  nil nil nil]
                                                [nil nil nil \1  \5  \3  nil nil nil]
                                                [nil nil nil \7  \9  \2  nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] '(1 1) \4))))
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
           (assign-number-in-quadrant          [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil \6  nil \8  nil nil nil]
                                                [nil nil nil \1  \5  \3  nil nil nil]
                                                [nil nil nil \7  \9  \2  nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] '(1 1) \8))))
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
           (assign-number-in-quadrant          [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil \1  nil nil nil]
                                                [nil nil nil \1  nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil \1  nil nil]] '(2 1) \1))))

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
           (assign-number-in-quadrant          [[nil nil nil nil nil nil nil nil nil]
                                                [\5  \6  \2  \8  nil  \4  \9  \3  \7]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] '(0 1) \1))))

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
           (assign-number-in-quadrant           [[nil nil nil nil \7  nil nil nil nil]
                                                 [nil nil nil nil nil nil nil nil nil]
                                                 [nil nil nil nil \5  nil nil nil nil]
                                                 [nil nil nil nil \2  nil nil nil nil]
                                                 [nil nil nil nil \8  nil nil nil nil]
                                                 [nil nil nil nil \9  nil nil nil nil]
                                                 [nil nil nil nil \6  nil nil nil nil]
                                                 [nil nil nil nil \4  nil nil nil nil]
                                                 [nil nil nil nil \3  nil nil nil nil]] '(0 1) \1))))

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
           (assign-number-in-quadrant           [[\3  \9  nil \8  nil \6  nil \5  nil]
                                                 [nil nil \6  nil \1  nil \3  \8  nil]
                                                 [nil \8  nil nil \3  nil \9  nil \6]
                                                 [\1  \3  \9  nil nil \8  \4  \6  \2]
                                                 [nil nil nil \6  \2  \3  \8  \1  \9]
                                                 [\8  \6  \2  nil nil nil \7  \3  \5]
                                                 [\6  nil \3  nil \8  nil nil \7  nil]
                                                 [\9  \7  \8  nil \6  nil nil nil \3]
                                                 [nil \5  nil \3  nil nil \6  \9  \8]] '(1 1) \9))))

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
           (assign-number-in-quadrant           [[\3  \9  nil \8  nil \6  nil \5  nil]
                                                 [nil nil \6  nil \1  nil \3  \8  nil]
                                                 [nil \8  nil nil \3  nil \9  nil \6]
                                                 [\1  \3  \9  \7  \5  \8  \4  \6  \2]
                                                 [nil nil nil \6  \2  \3  \8  \1  \9]
                                                 [\8  \6  \2  nil \9  nil \7  \3  \5]
                                                 [\6  nil \3  nil \8  nil nil \7  nil]
                                                 [\9  \7  \8  nil \6  nil nil nil \3]
                                                 [nil \5  nil \3  nil nil \6  \9  \8]] '(0 2) \2))))

  (testing "assigns number 3 in seventh quadrant by process of elimination"
    (is (= [[nil nil nil \2  nil \6  nil \8  \3 ]
            [nil \2  \3  \7  nil nil nil nil nil]
            [\5  nil nil nil nil nil \7  nil nil]
            [nil nil \4  nil nil \2  nil nil \1 ]
            [nil \8  nil nil nil nil nil \7  nil]
            [\9  nil nil \1  nil nil \2  nil nil]
            [\4  \9  \5  \8  \2  \7  nil nil \6 ]
            [nil \3 nil  \6  \1  \5  \9  \4  \7 ]
            [\7  \1  \6  \9  \4  \3  nil nil nil]]
           (assign-number-in-quadrant [[nil nil nil \2  nil \6  nil \8  \3 ]
                                       [nil \2  \3  \7  nil nil nil nil nil]
                                       [\5  nil nil nil nil nil \7  nil nil]
                                       [nil nil \4  nil nil \2  nil nil \1 ]
                                       [nil \8  nil nil nil nil nil \7  nil]
                                       [\9  nil nil \1  nil nil \2  nil nil]
                                       [\4  \9  \5  \8  \2  \7  nil nil \6 ]
                                       [nil nil nil \6  \1  \5  \9  \4  \7 ]
                                       [\7  \1  \6  \9  \4  \3  nil nil nil]] '(2 0) \3)))))

(deftest assign-number-in-row-test
  (testing "assigns-number-in-row-when-that-number-completes-the-row"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [\5  \6  \2  \8  \1  \4  \9  \3  \7 ]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-row               [[nil nil nil nil nil nil nil nil nil]
                                                [\5  \6  \2  \8  nil  \4  \9  \3  \7]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] 1 \1))))
  (testing "does nothing if it cannot assign a number in row"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [\5  \6  \2  \8  nil \4  \9  nil \7 ]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-row               [[nil nil nil nil nil nil nil nil nil]
                                                [\5  \6  \2  \8  nil  \4  \9 nil \7 ]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] 1 \1))))
  (testing "assigns number in column in row when other column in row already contains same number in different row"
    (is (= [[nil nil nil nil nil nil nil nil nil]
            [\5  nil \2  \8  \3  \4  \9  \1  \7 ]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]
            [nil \1  nil nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil nil]]
           (assign-number-in-row               [[nil nil nil nil nil nil nil nil nil]
                                                [\5  nil \2  \8  \3  \4  \9  nil \7 ]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil \1  nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil nil]] 1 \1))))
  (testing "assigns number 5 in quadrant 2 by observing that 5 is the only number left of the three to complete the row which can be assigned to coordinate (0,2)"
    (is (= [[nil \2  \4  \1  nil \5  \3  \7  \9 ]
            [\3  \1  nil nil nil \9  nil \6  \2 ]
            [nil \7  nil \2  nil \3  nil \4  \1 ]
            [\1  \6  \5  \8  \3  \2  \4  \9  \7 ]
            [nil \9  \3  nil nil nil \6  \2  nil]
            [nil \4  \2  \9  nil \6  \1  \3  nil]
            [nil \3  nil \5  nil \8  \2  \1  nil]
            [\2  \5  nil \6  nil nil \7  \8  \3 ]
            [nil \8  nil \3  \2  nil \9  \5  nil]]
           (assign-number-in-row      [[nil \2  \4  \1  nil nil \3  \7  \9 ]
                                       [\3  \1  nil nil nil \9  nil \6  \2 ]
                                       [nil \7  nil \2  nil \3  nil \4  \1 ]
                                       [\1  \6  \5  \8  \3  \2  \4  \9  \7 ]
                                       [nil \9  \3  nil nil nil \6  \2  nil]
                                       [nil \4  \2  \9  nil \6  \1  \3  nil]
                                       [nil \3  nil \5  nil \8  \2  \1  nil]
                                       [\2  \5  nil \6  nil nil \7  \8  \3 ]
                                       [nil \8  nil \3  \2  nil \9  \5  nil]] 0 \5)))))

(deftest assign-number-in-column-test
  (testing "assigns number in column when it completes the column"
    (is (= [[nil nil nil nil nil nil nil nil \6 ]
            [nil nil nil nil nil nil nil nil \3 ]
            [nil nil nil nil nil nil nil nil \2 ]
            [nil nil nil nil nil nil nil nil \8 ]
            [nil nil nil nil nil nil nil nil \9 ]
            [nil nil nil nil nil nil nil nil \5 ]
            [nil nil nil nil nil nil nil nil \7 ]
            [nil nil nil nil nil nil nil nil \4 ]
            [nil nil nil nil nil nil nil nil \1 ]]
           (assign-number-in-column            [[nil nil nil nil nil nil nil nil \6 ]
                                                [nil nil nil nil nil nil nil nil \3 ]
                                                [nil nil nil nil nil nil nil nil \2 ]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil \9 ]
                                                [nil nil nil nil nil nil nil nil \5 ]
                                                [nil nil nil nil nil nil nil nil \7 ]
                                                [nil nil nil nil nil nil nil nil \4 ]
                                                [nil nil nil nil nil nil nil nil \1 ]] 8 \8))))

  (testing "does nothing if it cannot assign a number in column"
    (is (= [[nil nil nil nil nil nil nil nil nil ]
            [nil nil nil nil nil nil nil nil \3 ]
            [nil nil nil nil nil nil nil nil \2 ]
            [nil nil nil nil nil nil nil nil nil ]
            [nil nil nil nil nil nil nil nil \9 ]
            [nil nil nil nil nil nil nil nil \5 ]
            [nil nil nil nil nil nil nil nil \7 ]
            [nil nil nil nil nil nil nil nil \4 ]
            [nil nil nil nil nil nil nil nil \1 ]]
           (assign-number-in-column            [[nil nil nil nil nil nil nil nil nil ]
                                                [nil nil nil nil nil nil nil nil \3 ]
                                                [nil nil nil nil nil nil nil nil \2 ]
                                                [nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil \9 ]
                                                [nil nil nil nil nil nil nil nil \5 ]
                                                [nil nil nil nil nil nil nil nil \7 ]
                                                [nil nil nil nil nil nil nil nil \4 ]
                                                [nil nil nil nil nil nil nil nil \1 ]] 8 \8))))

  (testing "assigns number in row in column when other row in column already contains same number in different column"
    (is (= [[nil nil nil nil nil nil nil nil \8 ]
            [nil nil nil nil nil nil nil nil \3 ]
            [nil nil nil nil nil nil nil nil \2 ]
            [nil nil \8  nil nil nil nil nil nil]
            [nil nil nil nil nil nil nil nil \9 ]
            [nil nil nil nil nil nil nil nil \5 ]
            [nil nil nil nil nil nil nil nil \7 ]
            [nil nil nil nil nil nil nil nil \4 ]
            [nil nil nil nil nil nil nil nil \1 ]]
           (assign-number-in-column            [[nil nil nil nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil \3 ]
                                                [nil nil nil nil nil nil nil nil \2 ]
                                                [nil nil \8  nil nil nil nil nil nil]
                                                [nil nil nil nil nil nil nil nil \9 ]
                                                [nil nil nil nil nil nil nil nil \5 ]
                                                [nil nil nil nil nil nil nil nil \7 ]
                                                [nil nil nil nil nil nil nil nil \4 ]
                                                [nil nil nil nil nil nil nil nil \1 ]] 8 \8))))

  (testing "assigns number 5 in quadrant 4 by observing that 5 is the only number left of the three to complete the row which can be assigned to coordinate (2,0)"
    (is (= [[nil \3  nil \1  nil nil nil \2  nil]
            [\2  \1  \7  \6  \9  \4  \3  \5  \8 ]
            [\4  nil nil \5  \3  \2  nil nil nil]
            [\1  nil \2  \8  nil \9  \5  \6  \3 ]
            [nil nil nil \3  nil nil nil nil \2 ]
            [\5  \9  \3  \2  nil \6  \8  nil nil]
            [\3  nil nil \4  \6  \1  \2  \7  \9 ]
            [\7  \6  \4  \9  \2  \3  \1  \8  \5 ]
            [\9  \2  \1  \7  nil nil nil \3  nil]]
           (assign-number-in-column   [[nil \3  nil \1  nil nil nil \2  nil]
                                       [\2  \1  \7  \6  \9  \4  \3  \5  \8 ]
                                       [\4  nil nil \5  \3  \2  nil nil nil]
                                       [\1  nil \2  \8  nil \9  \5  \6  \3 ]
                                       [nil nil nil \3  nil nil nil nil \2 ]
                                       [nil \9  \3  \2  nil \6  \8  nil nil]
                                       [\3  nil nil \4  \6  \1  \2  \7  \9 ]
                                       [\7  \6  \4  \9  \2  \3  \1  \8  \5 ]
                                       [\9  \2  \1  \7  nil nil nil \3  nil]] 0 \5)))))

(deftest columns-containing-number-test
  (testing "finds no columns containg number"
    (is (= #{}
           (columns-containing-number [[nil nil nil nil nil nil nil nil nil]
                                       [\5  nil nil nil nil nil \8  nil \6 ]
                                       [nil nil nil nil \2  nil nil nil nil]
                                       [nil nil \4  nil nil \9  nil nil nil]
                                       [nil nil nil nil nil nil nil \5  nil]
                                       [nil nil nil nil nil nil nil nil \8 ]
                                       [nil nil nil \8  \1  nil nil nil nil]
                                       [nil \1  nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil \2  nil nil]] \7))))
  (testing "finds single column containg number"
    (is (= #{4}
           (columns-containing-number [[nil nil nil nil nil nil nil nil nil]
                                       [\5  nil nil nil nil nil \8  nil \6 ]
                                       [nil nil nil nil \2  nil nil nil nil]
                                       [nil nil \4  nil nil \9  nil nil nil]
                                       [nil nil nil nil nil nil nil \5  nil]
                                       [nil nil nil nil nil nil nil nil \8 ]
                                       [nil nil nil \8  \1  nil nil nil nil]
                                       [nil \1  nil nil \7  nil nil nil nil]
                                       [nil nil nil nil nil nil \2  nil nil]] \7))))
  (testing "finds multiple columns containg number"
    (is (= #{4 8}
           (columns-containing-number [[nil nil nil nil nil nil nil nil nil]
                                       [\5  nil nil nil nil nil \8  nil \6 ]
                                       [nil nil nil nil \2  nil nil nil nil]
                                       [nil nil \4  nil nil \9  nil nil \7 ]
                                       [nil nil nil nil nil nil nil \5  nil]
                                       [nil nil nil nil nil nil nil nil \8 ]
                                       [nil nil nil \8  \1  nil nil nil nil]
                                       [nil \1  nil nil \7  nil nil nil nil]
                                       [nil nil nil nil nil nil \2  nil nil]] \7)))))

(deftest row-containing-number-test
  (testing "finds no rows containg number"
    (is (= #{}
           (rows-containing-number [[nil nil nil nil nil nil nil nil nil]
                                       [\5  nil nil nil nil nil \8  nil \6 ]
                                       [nil nil nil nil \2  nil nil nil nil]
                                       [nil nil \4  nil nil \9  nil nil nil]
                                       [nil nil nil nil nil nil nil \5  nil]
                                       [nil nil nil nil nil nil nil nil \8 ]
                                       [nil nil nil \8  \1  nil nil nil nil]
                                       [nil \1  nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil \2  nil nil]] \7))))

  (testing "finds single row containg number"
    (is (= #{7}
           (rows-containing-number [[nil nil nil nil nil nil nil nil nil]
                                       [\5  nil nil nil nil nil \8  nil \6 ]
                                       [nil nil nil nil \2  nil nil nil nil]
                                       [nil nil \4  nil nil \9  nil nil nil]
                                       [nil nil nil nil nil nil nil \5  nil]
                                       [nil nil nil nil nil nil nil nil \8 ]
                                       [nil nil nil \8  \1  nil nil nil nil]
                                       [nil \1  nil nil \7  nil nil nil nil]
                                       [nil nil nil nil nil nil \2  nil nil]] \7))))
  (testing "finds multiple rows containg number"
    (is (= #{3 7}
           (rows-containing-number [[nil nil nil nil nil nil nil nil nil]
                                       [\5  nil nil nil nil nil \8  nil \6 ]
                                       [nil nil nil nil \2  nil nil nil nil]
                                       [nil nil \4  nil nil \9  nil nil \7 ]
                                       [nil nil nil nil nil nil nil \5  nil]
                                       [nil nil nil nil nil nil nil nil \8 ]
                                       [nil nil nil \8  \1  nil nil nil nil]
                                       [nil \1  nil nil \7  nil nil nil nil]
                                       [nil nil nil nil nil nil \2  nil nil]] \7)))))

(deftest possible-columns-for-number-in-row-test
  (testing "possible-columns-for-number-in-row returns column where number already exists"
    (is (= #{4} (possible-columns-for-number-in-row
                 [[nil nil nil nil nil nil nil nil nil]
                  [\5  nil nil nil nil nil \8  nil \6 ]
                  [nil nil nil nil \2  nil nil nil nil]
                  [nil nil \4  nil nil \9  nil nil \7 ]
                  [nil nil nil nil nil nil nil \5  nil]
                  [nil nil nil nil nil nil nil nil \8 ]
                  [nil nil nil \8  \1  nil nil nil nil]
                  [nil \1  nil nil \7  nil nil nil nil]
                  [nil nil nil nil nil nil \2  nil nil]] 7 \7)))))


(deftest reserved-columns-for-row-test
  (testing "Correctly detects two reserved columns"
    (is (= #{2 4} (reserved-columns-for-row (list (hash-set 2 4) (hash-set 2 4)))))))


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
           (sibling-eliminated-coordinates   [[nil nil nil nil nil \1  nil nil nil]
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
    (is (= true (number-at-coordinates-in-quadrant-completes-row?           [[nil nil nil nil nil nil nil nil nil]
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
  (testing "solve-puzzle solves the medium puzzle"
    (is (= [[\3  \9  \7  \8  \4  \6  \2  \5  \1 ]
            [\4  \2  \6  \5  \1  \9  \3  \8  \7 ]
            [\5  \8  \1  \2  \3  \7  \9  \4  \6 ]
            [\1  \3  \9  \7  \5  \8  \4  \6  \2 ]
            [\7  \4  \5  \6  \2  \3  \8  \1  \9 ]
            [\8  \6  \2  \1  \9  \4  \7  \3  \5 ]
            [\6  \1  \3  \9  \8  \2  \5  \7  \4 ]
            [\9  \7  \8  \4  \6  \5  \1  \2  \3 ]
            [\2  \5  \4  \3  \7  \1  \6  \9  \8 ]]
           (solve-puzzle [[nil \9  nil nil nil \6  nil \5  nil]
                          [nil nil nil nil \1  nil \3  \8  nil]
                          [nil \8  nil nil \3  nil nil nil \6 ]
                          [\1  \3  \9  nil nil nil \4  \6  nil]
                          [nil nil nil \6  nil \3  nil nil nil]
                          [nil \6  \2  nil nil nil \7  \3  \5 ]
                          [\6  nil nil nil \8  nil nil \7  nil]
                          [nil \7  \8  nil \6  nil nil nil nil]
                          [nil \5  nil \3  nil nil nil \9  nil]]))))
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

(deftest reserved-coordinates-within-quadrant-test
  (testing "reserved-coordinates-within-quadrant identifies two spaces should be reserved if two numbers hsve possible coordinates of only those two coordinates"
    (is (= (hash-set (list \1 \0) (list \1 \2))
           (reserved-coordinates-within-quadrant
            (list (hash-set (list \1 \0) (list \1 \2)) (hash-set (list \1 \0) (list \1 \2))))))))

(deftest row-contains-number?-test
  (testing "row-contains-number?"
    (is (= true (row-contains-number? [[nil nil nil nil nil nil nil nil nil]
                                       [\5  \6  \2  \8  nil  \4  \9  \3  \7]
                                       [nil nil nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil nil nil nil]
                                       [nil nil nil nil nil nil nil nil nil]], 1, \8)))
    (is (= false (row-contains-number? [[nil nil nil nil nil nil nil nil nil]
                                        [\5  \6  \2  \8  nil  \4  \9  \3  \7]
                                        [nil nil nil nil nil nil nil nil nil]
                                        [nil nil nil nil nil nil nil nil nil]
                                        [nil nil nil nil nil nil nil nil nil]
                                        [nil nil nil nil nil nil nil nil nil]
                                        [nil nil nil nil nil nil nil nil nil]
                                        [nil nil nil nil nil nil nil nil nil]
                                        [nil nil nil nil nil nil nil nil nil]], 1, \1)))))

(deftest columns-in-row-containing-number-test
  (testing "columns-in-row-containing-number"
    (is (= 5 (column-in-row-containing-number [\2 nil \5 \3 nil \9 \1 \8 nil] \9)))
    (is (= 0 (column-in-row-containing-number [\2 nil \5 \3 nil \9 \1 \8 nil] \2)))
    (is (= nil (column-in-row-containing-number [\2 nil \5 \3 nil \9 \1 \8 nil] \4)))))

(deftest get-column-test
  (testing "gets the column from the puzzle"
    (is (= [\3 \5 \8 nil \9 \1 \4 \7 nil]
           (get-column [[nil \3  nil \1  nil \3  nil \2  nil]
                        [\2  \1  \7  \6  \9  \5  \3  \5  \8 ]
                        [\4  nil nil \5  \3  \8  nil nil nil]
                        [\1  nil \2  \8  nil nil \5  \6  \3 ]
                        [nil nil nil \3  nil \9  nil nil \2 ]
                        [nil \9  \3  \2  nil \1  \8  nil nil]
                        [\3  nil nil \4  \6  \4  \2  \7  \9 ]
                        [\7  \6  \4  \9  \2  \7  \1  \8  \5 ]
                        [\9  \2  \1  \7  nil nil nil \3  nil]] 5)))))
