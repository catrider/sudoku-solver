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
           (get-quadrant '(0 0)
                         [[nil \6 nil nil nil \5 nil \1 \9]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [\3 nil \9 \8 \1 nil nil nil \8]]))))
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
           (get-quadrant '(0 1)
                         [[nil \6 nil nil nil \5 nil \1 \9]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [\3 nil \9 \8 \2 nil nil nil \8]]))))
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
           (get-quadrant '(2 1)
                         [[nil \6 nil nil nil \5 nil \1 \9]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil \2 \3 nil \1 nil nil nil]
                          [nil nil nil \5 \9 nil nil nil nil]
                          [nil nil nil nil \2 nil nil nil nil]
                          [nil nil nil \8 nil \3 nil nil nil]])))))
