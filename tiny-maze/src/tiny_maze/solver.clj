(ns tiny-maze.solver)


(def example [[:S 0 1]
              [1  0 1]
              [1  0 :E]])

(def example2 [[:S 0 0 1]
               [1  1 0 0]
               [1  0  0 1]
               [1  1  0 :E]])

(def example3 [[:S 0 0 0]
               [1  0 1 0]
               [0  0 1 0]
               [0  0 0 :E]])

(defn maze-dimensions [maze]
  "Returns the dimensions of the maze"
  [(count maze) (count (first maze))])

(defn all-possible-neighbors [[x y]]
  "gets all of the neighbors of a pos"
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn within-maze? [maze loc]
  "determines whether a location is inside the maze"
  (every? true? (map > (maze-dimensions maze) loc)))

(defn valid-step? [maze loc]
  "determines of a given location is a valid place to go for a given maze"
  (and (within-maze? maze loc)
       (contains? #{:E 0} (get-in maze loc))))

(defn possible-next-steps [maze loc]
  "given a maze and loc, find all neighboring locations that are valid steps"
  (filter #(valid-step? maze %) (all-possible-neighbors loc)))

(defn finished? [maze]
  "determine if the maze is complete (meaning, :E has been marked :x)"
  (-> maze
      (flatten)
      (set)
      (contains? :E)
      (not)))

(defn make-step [maze loc]
  "marks a location with :x"
  (update-in maze loc (constantly :x)))

(defn start-loc [maze]
  "find the :S location"
  (let [start-row (first (filter #(contains? (set %) :S) maze))]
    [(.indexOf start-row :S) (.indexOf maze start-row)]))

(defn walk-maze [maze loc]
  "returns an array of all the possible ways to solve the maze"
  (if (finished? maze)
    [maze]
    (let [next-steps (possible-next-steps maze loc)]
        (->> next-steps
             (map #(make-step maze %))
             (mapcat #(walk-maze %2 %1) next-steps))
        )))


(defn solve-maze [maze]
  "takes the first solution returned by walk-maze"
  (first (walk-maze (make-step maze (start-loc maze)) (start-loc maze))))
