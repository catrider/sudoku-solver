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

(defn- coordinates-from-index
  "Converts an index to coordiantes, assuming the quadrant dimension is 3"
  [index]
  (list (quot index 3) (mod index 3)))

(defn- seq-of-three
  [index coll]
  (take 3 (nthrest coll (* 3 index))))

(defn get-quadrant
  "Returns the nth quadrant in the form of a map"
  [[x y] puzzle]
  (apply (partial merge {\1 nil \2 nil \3 nil \4 nil \5 nil \6 nil \7 nil \8 nil \9 nil})
      (map-indexed (fn [idx itm] (if (nil? itm) {} (hash-map itm (coordinates-from-index idx))))
        (flatten (map #(seq-of-three y %)
            (seq-of-three x puzzle))))))

(defn- get-coordinates-of-number-in-quadrant
  [number quadrant puzzle]
  (get (get-quadrant quadrant puzzle) number))

(defn lateral-sibling-quadrants
  "returns quadrants that sit laterally to the passed quadrant"
  [[x y]]
  (set (map #(list x %)
       (filter #(not (= % y))
                     (range 3)))))

(defn vertical-sibling-quadrants
  "returns the quadrants that sit vertically from the passed quadrant"
  [[x y]]
  (set (map #(list % y)
         (filter #(not (= % x))
                       (range 3)))))

(defn assign-number-in-quadrant
  [[x y] number puzzle]
  ())
