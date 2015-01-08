(ns sudoku-solver.convert-test
  (:require [clojure.test :refer :all]
            [sudoku-solver.convert :refer :all]))

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
                              [\1  \7  \9  \8  \5  \6  \4  \2  \3]]))))
    (testing "display-puzzle replaces nil values with spaces"
      (is (= "| - - - - - - - - - - - |\n| 2   8 | 4 7 5 | 9 3 1 |\n| 4 5 1 | 9 6 3 | 2 7 8 |\n| 7 9 3 | 1 8 2 | 5 6   |\n| - - - | - - - | - - - |\n| 9 2 7 | 5 3 4 | 1 8 6 |\n| 3 1   | 6 9 8 | 7 4 2 |\n| 8 4 6 | 7 2 1 | 3 9 5 |\n| - - - | - - - | - - - |\n| 6 3 4 | 2 1   | 8 5 7 |\n| 5 8 2 | 3 4 7 | 6   9 |\n|   7 9 | 8 5 6 | 4 2 3 |\n| - - - - - - - - - - - |"
             (display-puzzle [[\2  nil  \8  \4  \7  \5  \9  \3  \1]
                              [\4  \5  \1  \9  \6  \3  \2  \7  \8]
                              [\7  \9  \3  \1  \8  \2  \5  \6  nil]
                              [\9  \2  \7  \5  \3  \4  \1  \8  \6]
                              [\3  \1  nil  \6  \9  \8  \7  \4  \2]
                              [\8  \4  \6  \7  \2  \1  \3  \9  \5]
                              [\6  \3  \4  \2  \1  nil  \8  \5  \7]
                              [\5  \8  \2  \3  \4  \7  \6  nil  \9]
                              [nil  \7  \9  \8  \5  \6  \4  \2  \3]])))))

  (deftest display-puzzle-row-test
    (testing "display-puzzle-row displays the puzzle row"
      (is (= "| 1 2 3 | 4 5 6 | 7 8 9 |"
             (display-puzzle-row [1 2 3 4 5 6 7 8 9])))))
