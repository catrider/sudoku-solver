(ns sudoku-solver.seq_utils)

(defn indices-of-seq-whose-vals-satisfy-cond
  [seq cond]
  (set
   (map
    (fn extract-index
      [[idx itm]]
      (identity idx))
    (filter
     (fn apply-cond
       [[idx itm]]
       (cond itm))
     (map-indexed
      (fn [idx itm]
        (list idx itm))
      seq)))))
