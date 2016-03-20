(ns wonderland-number.finder)

(def number-range (range 100000 1000000))

(defn hasAllTheSameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn wonderland-multiplication [number]
	(let [multiplicands (range 2 7)]
		(map #(* number %) multiplicands)))

(defn isAWonderlandNumber? [number]
	(every? true? (map #(hasAllTheSameDigits? number %) (wonderland-multiplication number))))

(defn wonderland-number []
  (first (filter isAWonderlandNumber? number-range)))
