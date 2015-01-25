(ns sudoku-solver.puzzle)
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[sudoku-solver.convert :as convert])
(require '[sudoku-solver.seq_utils :as seq_utils])
(require '[clojure.math.combinatorics :as combo])


(defn is-puzzle-complete?
  [puzzle]
  (not (reduce #(or %1 %2) (map nil? (flatten puzzle)))))

(declare assign-number-in-quadrant)
(declare assign-number-in-row)

(defn- int-to-char
  [i]
  (char (+ 48 i)))

(defn solve-puzzle
  "Solves the puzzle"
  [puzzle]
  (let [mutated-puzzle
        (reduce (fn [p [quadrant number]]
                  (assign-number-in-quadrant p quadrant number))
                puzzle
                (for [x (range 3)
                      y (range 3)
                      number (map int-to-char (range 1 10))]
                  (list (list x y) number)))]
    (let [mutated-puzzle-2
          (reduce (fn [p [row-idx number]]
                    (assign-number-in-row p row-idx number))
                  mutated-puzzle
                  (for [row-idx (range 9)
                        number (map int-to-char (range 1 10))]
                    (list row-idx number)))]
      (if (is-puzzle-complete? mutated-puzzle-2)
        mutated-puzzle-2
        (if (= mutated-puzzle-2 puzzle)
          (throw (Exception. (str "Could not solve puzzle. Got this far:\n" (convert/display-puzzle puzzle))))
          (solve-puzzle mutated-puzzle-2))))))

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

(defn- numbers-0-to-2-except
  "Retuns a sequence of numbers 0 through 2 except for the passed number"
  [n]
  (filter
   (partial not= n)
   (range 3)))

(defn- numbers-1-through-9-except
  "Returns a sequence of numbers 1 through 9 except for the passed number"
  [n]
  (disj
   #{\1 \2 \3 \4 \5 \6 \7 \8 \9}
   n))

(defn lateral-sibling-quadrants
  "returns quadrants that sit laterally to the passed quadrant"
  [[xq yq]]
  (set (map #(list xq %)
       (numbers-0-to-2-except yq))))

(defn vertical-sibling-quadrants
  "returns the quadrants that sit vertically from the passed quadrant"
  [[xq yq]]
  (set (map #(list % yq)
         (numbers-0-to-2-except xq))))

(defn- coordinates-of-numbers-in-sibling-quadrants
  "Returns coordinates of numbers in sibling quadrants"
  [puzzle quadrant number sibling-quadrants-fn]
  (remove nil?
          (map (fn coordinates-of-number-in-quadrant [quadrant-map] (get quadrant-map number))
               (map (partial get-quadrant puzzle)
                    (sibling-quadrants-fn quadrant)))))

(defn coordinates-eliminated-by-lateral-siblings
  [puzzle quadrant number]
  (reduce (fn accumulate [val a] (apply (partial conj val) a)) #{}
            (map (fn eliminated-coordinates-based-on-vertical-sibling-coordinates
                   [[x y :as coordinates]]
                   (list (list x 0) (list x 1) (list x 2)))
                 (coordinates-of-numbers-in-sibling-quadrants puzzle quadrant number lateral-sibling-quadrants))))

(defn coordinates-eliminated-by-vertical-siblings
  [puzzle quadrant number]
  (reduce (fn accumulate [val a] (apply (partial conj val) a)) #{}
            (map (fn eliminated-coordinates-based-on-lateral-sibling-coordinates
                   [[x y :as coordinates]]
                   (list (list 0 y) (list 1 y) (list 2 y)))
                 (coordinates-of-numbers-in-sibling-quadrants puzzle quadrant number vertical-sibling-quadrants))))

(defn sibling-eliminated-coordinates
  [puzzle quadrant number]
  (set
   (set/union
    (coordinates-eliminated-by-lateral-siblings puzzle quadrant number)
    (coordinates-eliminated-by-vertical-siblings puzzle quadrant number))))

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

(defn row-contains-number?
  [puzzle row-idx number]
  (boolean
   (some
    (fn [n]
      (= number n))
    (get puzzle row-idx))))

(defn reserved-coordinates-within-quadrant
  [coordinate-sets]
  (if (< 20 (reduce (fn total-coordinates [c s] (+ c (count s))) 0 coordinate-sets))
    (hash-set)
    (loop [unique-coordinates (reduce set/union coordinate-sets)
         reserved-coordinates (hash-set)]
      (if (empty? unique-coordinates)
        reserved-coordinates
        (let [uq (first unique-coordinates)]
          (recur
           (rest unique-coordinates)
           (if
             (every?
              (fn assignment-set-is-invalid
                [assignment-set]
                (not= (count coordinate-sets) (count assignment-set)))
              (map
               set
               (apply
                combo/cartesian-product
                (remove
                 empty?
                 (map
                  (fn [cs] (disj cs uq))
                  coordinate-sets)))))
             (conj reserved-coordinates uq)
             reserved-coordinates)))))))

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

(declare possible-coordinates-for-number-in-quadrant)

(defn- reserved-coordinates-based-on-lateral-siblings
  "Returns a set of coordinates that are reserved in the passed quadrant based on its lateral siblings"
  [puzzle quadrant number depth]
  (reserved-coordinates
   (reduce
    (fn [coll q]
      (conj coll (possible-coordinates-for-number-in-quadrant puzzle q number (inc depth))))
    '()
    (filter #(not (quadrant-contains-number? puzzle % number)) (lateral-sibling-quadrants quadrant)))
   false))

(defn- reserved-coordinates-based-on-vertical-siblings
  "Returns a set of coordinates that are reserved in the passed quadrant based on its vertical siblings"
  [puzzle quadrant number depth]
  (reserved-coordinates
   (reduce
    (fn [coll q]
      (conj coll (possible-coordinates-for-number-in-quadrant puzzle q number (inc depth))))
    '()
    (filter #(not (quadrant-contains-number? puzzle % number)) (vertical-sibling-quadrants quadrant)))
   true))

(defn- possible-coordinates-for-number-in-quadrant-simple
  [puzzle quadrant number]
  (set/difference
   #{'(0 0) '(0 1) '(0 2) '(1 0) '(1 1) '(1 2) '(2 0) '(2 1) '(2 2)}
   (sibling-eliminated-coordinates puzzle quadrant number)
   (set (remove nil? (vals (get-quadrant puzzle quadrant))))))

(defn- possible-coordinates-for-number-in-quadrant
  ([puzzle [qx qy :as quadrant] number]
   (possible-coordinates-for-number-in-quadrant puzzle quadrant number 1))
  ([puzzle [qx qy :as quadrant] number depth]
   (reduce
    (fn [possible-coordinates reserved-coordinates-fn]
      (if (and (> (count possible-coordinates) 1) (< depth 3))
        (set/difference possible-coordinates (reserved-coordinates-fn))
        possible-coordinates))
    (set/difference
     #{'(0 0) '(0 1) '(0 2) '(1 0) '(1 1) '(1 2) '(2 0) '(2 1) '(2 2)}
     (set (remove nil? (vals (get-quadrant puzzle quadrant))))
     (sibling-eliminated-coordinates puzzle quadrant number))
    (list (fn [] (reserved-coordinates-based-on-vertical-siblings puzzle quadrant number (inc depth)))
          (fn [] (reserved-coordinates-based-on-lateral-siblings puzzle quadrant number (inc depth)))
          (fn [] (reserved-coordinates-within-quadrant (map
                                                        (partial possible-coordinates-for-number-in-quadrant-simple puzzle quadrant)
                                                        (numbers-1-through-9-except number))))))))


(defn assign-number-in-quadrant
  [puzzle [x y :as quadrant] number]
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

(defn occupied-columns-in-row
  [row]
  (seq_utils/indices-of-seq-whose-vals-satisfy-cond row (complement nil?)))

(defn- assign-number-at-row-and-column
  [puzzle row-idx column-idx number]
  (assign-at-coordinates puzzle (list (quot row-idx 3) (quot column-idx 3)) (list (mod row-idx 3) (mod column-idx 3)) number))

(defn reserved-columns-for-row
  [column-sets]
  (reserved-coordinates-within-quadrant column-sets))

(defn columns-containing-number
  [puzzle number]
  (set
   (map
    (fn [[column numbers]]
      (identity column))
    (filter
     (fn [[column numbers]]
       (contains? numbers number))
     (map-indexed
      (fn [idx itm]
        (list idx (set itm)))
      (map
       (fn [column-idx]
         (map
          (fn [row]
            (get row column-idx))
          puzzle))
       (range 9)))))))

(defn column-in-row-containing-number
  [row number]
  (first
   (map
    (fn [[idx n]]
      (identity idx))
    (filter
     (fn [[idx n]]
       (= n number))
     (map-indexed
      (fn [idx number]
        (list idx number))
      row)))))

(defn possible-columns-for-number-in-row
  ([puzzle row-idx number]
   (possible-columns-for-number-in-row puzzle row-idx number 1))
  ([puzzle row-idx number depth]
   (if (row-contains-number? puzzle row-idx number)
     (hash-set (column-in-row-containing-number (get puzzle row-idx) number))
     (reduce
      (fn [possible-columns reserved-columns-fn]
        (if (and (> (count possible-columns) 1) (< depth 3))
          (set/difference possible-columns (reserved-columns-fn))
          possible-columns))
      (set/difference
       (set (range 9))
       (occupied-columns-in-row (get puzzle row-idx)))
      (list (fn [] (columns-containing-number puzzle number))
            (fn [] (reserved-columns-for-row (map
                                              (fn [n]
                                                (possible-columns-for-number-in-row puzzle row-idx n (inc depth)))
                                              (numbers-1-through-9-except number)))))))))

(defn assign-number-in-row
  [puzzle row-idx number]
  (let [possible-columns-for-number-in-row
        (possible-columns-for-number-in-row puzzle row-idx number)]
    (if (= 1 (count possible-columns-for-number-in-row))
      (assign-number-at-row-and-column puzzle row-idx (first possible-columns-for-number-in-row) number)
      puzzle)))

(defn assign-number-in-column
  [puzzle column-idx number]
  ())
