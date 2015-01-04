(ns sudoku-solver.core)
(require '[clojure.string :as str])
(require '[clojure.set :as set])

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

(defn is-puzzle-complete?
  [puzzle]
  (not (reduce #(or %1 %2) (map nil? (flatten puzzle)))))

(declare assign-number-in-quadrant)

(defn- int-to-char
  [i]
  (char (+ 48 i)))

(defn solve-puzzle
  "Solves the puzzle"
  [puzzle]
  (let [mutated-puzzle
        (reduce (fn mutated-puzzle
                  [p args]
                  (assign-number-in-quadrant (first args) (last args) p))
                puzzle
                (for [x (range 3)
                      y (range 3)
                      number (map int-to-char (range 1 10))]
                  (list (list x y) number)))]
    (if (is-puzzle-complete? mutated-puzzle)
      mutated-puzzle
      (if (= mutated-puzzle puzzle)
        (throw (Exception. (str "Could not solve puzzle. Got this far:\n" puzzle)))
        (solve-puzzle mutated-puzzle)))))

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

(defn- index-for-coordinates-in-quadrant
  [qi ci]
  (+ (* 3 qi) ci))

(defn- assign-at-coordinates
  [puzzle [qx qy :as quadrant] [cx cy :as coordinates] number]
  (assoc puzzle (index-for-coordinates-in-quadrant qx cx) (assoc (nth puzzle (index-for-coordinates-in-quadrant qx cx)) (index-for-coordinates-in-quadrant qy cy) number)))

(defn- numbers-in-row-of-quadrant-and-coordinates
  [puzzle [qx qy :as quadrant] [cx cy :as coordinate]]
  (set (nth puzzle (+ (* 3 qx) cx))))

(defn- numbers-in-column-of-quadrant-and-coordinates
  [puzzle [qx qy :as quadrant] [cx cy :as coordinate]]
  (set
   (map
    (fn number-in-column
      [row]
      (nth row (index-for-coordinates-in-quadrant qy cy)))
    puzzle)))

(defn- line-is-complete?
  [numbers-in-row]
  (and (= 9 (count numbers-in-row)) (not (reduce #(or %1 %2) (map nil? numbers-in-row)))))

(defn number-at-coordinates-in-quadrant-completes-row?
  [puzzle [qx qy :as quadrant] [cx cy :as coordinates] number]
  (let [numbers-in-row
        (numbers-in-row-of-quadrant-and-coordinates (assign-at-coordinates puzzle quadrant coordinates number) quadrant coordinates)]
    (line-is-complete? numbers-in-row)))

(defn number-at-coordinates-in-quadrant-completes-column?
  [puzzle [qx qy :as quadrant] [cx cy :as coordinates] number]
  (let [numbers-in-column (numbers-in-column-of-quadrant-and-coordinates (assign-at-coordinates puzzle quadrant coordinates number) quadrant coordinates)]
    (line-is-complete? numbers-in-column)))

(defn- row
  [[x y :as coordinates]]
  (identity x))

(defn- column
  [[x y :as coordinates]]
  (identity y))

(defn- rows
  [coordinates]
  (set (map row coordinates)))

(defn columns
  [coordinates]
  (set (map column coordinates)))

(defn quadrant-contains-number?
  [puzzle quadrant number]
  (not (nil? (get (get-quadrant puzzle quadrant) number))))

(defn reserved-coordinates
  [coordinate-sets vertical?]
  (if (identity vertical?)
    (loop [possible-columns (group-by count (map columns coordinate-sets))
           reserved-columns #{}
           c 1]
      (if (> c 2)
        (set (mapcat #(list (list 0 %) (list 1 %) (list 2 %)) reserved-columns))
        (let [new-reserved-columns (filter
                                    (fn [x]
                                      (or
                                       (= 1 (count x))
                                       (= 2 ((frequencies (possible-columns c)) x))))
                                      (possible-columns c))]
          (recur (group-by
                  count
                  (map
                   columns
                   (map
                    (fn [coordinates]
                      (filter
                       (fn [coordinate] (not (contains? (apply set/union new-reserved-columns) (column coordinate))))
                       coordinates))
                    coordinate-sets)))
                 ((partial apply set/union reserved-columns) new-reserved-columns)
                 (inc c)))))
    (loop [possible-rows (group-by count (map rows coordinate-sets))
           reserved-rows #{}
           c 1]
      (if (> c 2)
        (set (mapcat #(list (list % 0) (list % 1) (list % 2)) reserved-rows))
        (let [new-reserved-rows (filter
                                    (fn [x]
                                      (or
                                       (= 1 (count x))
                                       (= 2 ((frequencies (possible-rows c)) x))))
                                      (possible-rows c))]
          (recur (group-by
                  count
                  (map
                   rows
                   (map
                    (fn [coordinates]
                      (filter
                       (fn [coordinate] (not (contains? (apply set/union new-reserved-rows) (row coordinate))))
                       coordinates))
                    coordinate-sets)))
                 ((partial apply set/union reserved-rows) new-reserved-rows)
                 (inc c)))))))

(defn- possible-coordinates-for-number-in-quadrant
  ([puzzle [qx qy :as quadrant] number]
   (possible-coordinates-for-number-in-quadrant puzzle quadrant number 1))
  ([puzzle [qx qy :as quadrant] number depth]
   (let [ipc
         (set/difference
          #{'(0 0) '(0 1) '(0 2) '(1 0) '(1 1) '(1 2) '(2 0) '(2 1) '(2 2)}
          (sibling-eliminated-coordinates puzzle quadrant number)
          (set (remove nil? (vals (get-quadrant puzzle quadrant)))))]
     (if (< depth 3)
       (set/difference
        ipc
        (reserved-coordinates
         (reduce
          (fn [coll q]
            (conj coll (possible-coordinates-for-number-in-quadrant puzzle q number (inc depth))))
          '()
          (filter #(not (quadrant-contains-number? puzzle % number)) (vertical-sibling-quadrants quadrant)))
         true)
        (reserved-coordinates
         (reduce
          (fn [coll q]
            (conj coll (possible-coordinates-for-number-in-quadrant puzzle q number (inc depth))))
          '()
          (filter #(not (quadrant-contains-number? puzzle % number)) (lateral-sibling-quadrants quadrant)))
         false))
       ipc))))

(defn assign-number-in-quadrant
  [[x y :as quadrant] number puzzle]
  (if (quadrant-contains-number? puzzle quadrant number)
    puzzle
    (let [possible-coordinates-for-number
          (possible-coordinates-for-number-in-quadrant puzzle quadrant number)]
      (if (= 1 (count possible-coordinates-for-number))
        (assign-at-coordinates puzzle quadrant (first possible-coordinates-for-number) number)
        (let [coordinates-with-number-completes-row-or-column
              (filter
               (fn number-at-coordinate-completes-row-or-column
                 [coordinates]
                 (or
                  (number-at-coordinates-in-quadrant-completes-row? puzzle quadrant coordinates number)
                  (number-at-coordinates-in-quadrant-completes-column? puzzle quadrant coordinates number)))
               possible-coordinates-for-number)]
          (if (= 1 (count coordinates-with-number-completes-row-or-column))
            (assign-at-coordinates puzzle quadrant (first coordinates-with-number-completes-row-or-column) number)
            puzzle))))))
