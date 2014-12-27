(ns sudoku-solver.core)
(require '[clojure.string :as str])

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn parse-puzzle-file
  "Generate a list of lists from the sudoku puzzle file"
  [filepath]
  (vec (map #(vec %)
   (map #(map (fn spaces-into-nils [c] (if (= \space c) nil c)) %)
       (map #(filter (fn not-a-divider [c] (not (= \| c))) %)
         (map #(flatten (partition 1 2 %))
              (filter #(not (= \- (nth % 2)))
                (map #(seq %)
                   (str/split (slurp filepath) #"\n")))))))))

(defn solve-puzzle
  "Solves the puzzle"
  [puzzle]
  ())

(defn get-quadrant
  "Returns the nth quadrant in the form of a map"
  [n puzzle]
  ())
