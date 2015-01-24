(ns sudoku-solver.seq_utils_test
  (:require [clojure.test :refer :all]
            [sudoku-solver.seq_utils :refer :all]))

(deftest indices-of-seq-whose-vals-satisfy-cond-test
  (testing "works when a single val satisfies condition"
    (is (=
         #{2}
         (indices-of-seq-whose-vals-satisfy-cond
          '(4 2 5 4 1)
          (fn [v] (> v 4))))))
  (testing "works when a first val satisfies condition"
    (is (=
         #{0}
         (indices-of-seq-whose-vals-satisfy-cond
          '(5 2 4 4 1)
          (fn [v] (> v 4))))))
  (testing "works when a last val satisfies condition"
    (is (=
         #{4}
         (indices-of-seq-whose-vals-satisfy-cond
          '(3 2 4 4 5)
          (fn [v] (> v 4))))))
  (testing "works when multiple vals satisfy condition"
    (is (=
         #{0 3 4}
         (indices-of-seq-whose-vals-satisfy-cond
          '(6 2 4 9 5)
          (fn [v] (> v 4))))))
  (testing "works when no vals satisfy condition"
    (is (=
         #{}
         (indices-of-seq-whose-vals-satisfy-cond
          '(4 2 4 3 1)
          (fn [v] (> v 4)))))))
