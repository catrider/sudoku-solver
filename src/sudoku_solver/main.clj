(ns sudoku-solver.main
  (:gen-class))
(require '[sudoku-solver.convert :as convert])
(require '[sudoku-solver.puzzle :as puzzle])

(defn -main
  [& args]
  (println (convert/display-puzzle (puzzle/solve-puzzle (convert/parse-puzzle-file (first args))))))
