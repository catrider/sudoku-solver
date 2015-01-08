(ns sudoku-solver.convert)
(require '[clojure.string :as str])

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

(defn display-puzzle-row
  [numbers-row]
  (apply
   str
   (vec
    (interpose
     \space
     (flatten
      (interpose
       (flatten
        (interpose
         \|
         (partition 3 (replace {nil \space} numbers-row))))
       [\| \|]))))))

(defn display-puzzle
  "Displays a puzzle"
  [puzzle]
  (apply str
         (drop-last
          (flatten
           (interpose
            (interpose
             [\| \space \- \space \- \space \- \space \| \space \- \space \- \space \- \space \| \space \- \space \- \space \- \space \| \newline]
             (partition
              3
              (map
               (comp (fn [row] (concat row [\newline])) display-puzzle-row)
               puzzle)))
            (repeat 2 [\| \space \- \space \- \space \- \space \- \space \- \space \- \space \- \space \- \space \- \space \- \space \- \space \| \newline]))))))
