(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn sum-col [m]
  (map #(reduce + %) m))

(defn count-square [square]
  (count (flatten square)))

(defn get-remaining-nums [square]
  (remove (set (flatten square)) values))


(defn sum-diagonals-so-far [m]
  (filter number?
          [(if (> (count-square m) 8)
              (+ (first (first m)) (second (second m)) (last (last m))))
           (if (> (count-square m) 6)
             (+ (last (first m)) (second (second m)) (first (last m))))]))

(defn valid? [square]
  (if (> 3 (count-square square))
    true
    (let [done-rows (filter #(= 3 (count %)) square)
        done-cols (filter #(every? number? %) [(map first square)
                                               (map second square)
                                               (map #(rest (rest %)) square)])]
    (apply = (concat (sum-col done-rows)
                     (sum-col done-cols)
                     (sum-diagonals-so-far square))))))

(defn fill-square [nums square]
  (if (empty? nums)
    (if (and (valid? square)
             (= 9 (count-square square)))
      square)
    (let [current-row (int (Math/floor (/ (count-square square) 3)))
        first-try (vec (update-in square [current-row] #(concat % [(first nums)])))]
      (if (valid? first-try)
        (let  [next-try (fill-square (get-remaining-nums first-try) first-try)]
          (if next-try
            next-try
            (fill-square (rest nums) square)))
        (fill-square (rest nums) square)))))
;todo: move some of these if-checks to magic-square fn

(defn magic-square [values]
  (let [initial-square [[] [] []]]
    (vec (map vec (fill-square values initial-square)))))

