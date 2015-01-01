(ns sudoku-solver.core)
(require '[clojure.string :as str])
(require '[clojure.set :as set])

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
  [puzzle [x y]]
  (apply (partial merge {\1 nil \2 nil \3 nil \4 nil \5 nil \6 nil \7 nil \8 nil \9 nil})
      (map-indexed (fn [idx itm] (if (nil? itm) {} (hash-map itm (coordinates-from-index idx))))
        (flatten (map #(seq-of-three y %)
            (seq-of-three x puzzle))))))

(defn- not-equal-to-number
  [number]
  #(not (= number %)))

(defn lateral-sibling-quadrants
  "returns quadrants that sit laterally to the passed quadrant"
  [[x y]]
  (set (map #(list x %)
       (filter (not-equal-to-number y)
                     (range 3)))))

(defn vertical-sibling-quadrants
  "returns the quadrants that sit vertically from the passed quadrant"
  [[x y]]
  (set (map #(list % y)
         (filter (not-equal-to-number x)
                       (range 3)))))

(defn sibling-eliminated-coordinates
  [puzzle quadrant number]
  (set
   (set/union
    (reduce (fn accumulate [val a] (apply (partial conj val) a)) #{}
    (map (fn eliminated-coordinates-based-on-vertical-sibling-coordinates
                    [[x y :as coordinates]]
                    (list (list x 0) (list x 1) (list x 2)))
                  (remove nil?
                          (map (fn coordinates-of-number-in-quadrant [quadrant-map] (get quadrant-map number))
                               (map (partial get-quadrant puzzle)
                                    (lateral-sibling-quadrants quadrant))))))
    (reduce (fn accumulate [val a] (apply (partial conj val) a)) #{}
            (map (fn eliminated-coordinates-based-on-lateral-sibling-coordinates
                    [[x y :as coordinates]]
                    (list (list 0 y) (list 1 y) (list 2 y)))
                  (remove nil?
                          (map (fn coordinates-of-number-in-quadrant [quadrant-map] (get quadrant-map number))
                               (map (partial get-quadrant puzzle)
                                    (vertical-sibling-quadrants quadrant)))))))))

(defn- assign-at-coordinates
  [puzzle [qx qy :as quadrant] [cx cy :as coordinates] number]
  (assoc puzzle (+ (* 3 qx) cx) (assoc (nth puzzle (+ (* 3 qx) cx)) (+ (* 3 qy) cy) number)))

(defn number-at-coordinates-in-quadrant-completes-row?
  [puzzle [qx qy :as quadrant] [cx cy :as coordinates] number]
  (let [numbers-in-row (set (assoc (nth puzzle (+ (* 3 qx) cx)) (+ (* 3 qy) cy) number))]
    (and (= 9 (count numbers-in-row)) (not (contains? numbers-in-row nil)))))

(defn number-at-coordinates-in-quadrant-completes-column?
  [puzzle [qx qy :as quadrant] [cx cy :as coordinates] number]
  ())

(defn assign-number-in-quadrant
  [[x y :as quadrant] number puzzle]
  (if (not (nil? (get (get-quadrant puzzle quadrant) number)))
    puzzle
    (let [possible-coordinates-for-number
        (set/difference
           #{'(0 0) '(0 1) '(0 2) '(1 0) '(1 1) '(1 2) '(2 0) '(2 1) '(2 2)}
           (sibling-eliminated-coordinates puzzle quadrant number)
           (remove nil? (vals (get-quadrant puzzle quadrant))))]
    (if (= 1 (count possible-coordinates-for-number))
      (assign-at-coordinates puzzle quadrant (first possible-coordinates-for-number) number)
      (let [coordinates-with-number-completes-row-or-column
            (filter
             (fn number-at-coordinate-completes-row-or-column
               [coordinates]
               (or (number-at-coordinates-in-quadrant-completes-row? puzzle quadrant coordinates number) false)) possible-coordinates-for-number)]
        (if (= 1 (count coordinates-with-number-completes-row-or-column))
          (assign-at-coordinates puzzle quadrant (first coordinates-with-number-completes-row-or-column) number)
          puzzle))))))
