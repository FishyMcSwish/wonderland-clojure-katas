(ns tiny-maze.solver)


(def example [[:S 0 1]
              [1  0 1]
              [1  0 :E]])

(defn maze-dimensions [maze]
  [(count maze) (count (first maze))])

(defn all-possible-neighbors [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn within-maze? [maze loc]
  (every? true? (map > (maze-dimensions maze) loc)))

(defn valid-step? [maze loc]
  (and (within-maze? maze loc)
       (contains? #{:E 0} (get-in maze loc))))

(defn possible-next-steps [maze loc]
  (filter #(valid-step? maze %) (all-possible-neighbors loc)))

(defn finished? [maze]
  (-> maze
      (flatten)
      (set)
      (contains? :E)
      (not)))

(defn make-step [maze loc]
  (update-in maze loc (constantly :x)))

(defn start-loc [maze]
  (let [start-row (first (filter #(contains? (set %) :S) maze))]
    [(.indexOf start-row :S) (.indexOf maze start-row)]))

(defn walk-maze [maze loc]
  (if (finished? maze)
    maze
    (let [next-steps (possible-next-steps maze loc)]
      (first (->> next-steps
           (map #(make-step maze %))
           (map #(walk-maze %2 %1) next-steps))))))


(defn solve-maze [maze]
  (walk-maze (make-step maze (start-loc maze)) (start-loc maze)))
