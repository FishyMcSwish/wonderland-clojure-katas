(ns fox-goose-bag-of-corn.puzzle)


(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn repeating-positions? [newstep plan]
	(some #(= % (map set newstep)) (map #(map set %) plan)))


;todo: break this bad boy up
(defn is-safe? [newstep]
	(let [stepset (set (map set newstep))
		in-the-boat (set (second newstep))]
		(and (= (set (apply concat stepset)) #{:you :boat :fox :goose :corn})
			(not (or
				(contains? stepset #{:fox :goose})
				(contains? stepset #{:goose :corn})
				(< 3 (count in-the-boat))))
			(contains? in-the-boat :boat)
			(if (< 1 (count in-the-boat))
				(contains? in-the-boat :you)
				true))))

(defn valid-next-step? [plan newstep]
	(and (is-safe? newstep)
		(not (repeating-positions? newstep plan))))

(defn find-self [step]
	(first (filter #(contains? (set %) :you) step)))

(defn bring-across [step passengers]
	(let [step-minus-passengers (map (partial remove #(contains? (set passengers) %)) step)]
		[[(first step-minus-passengers) 
			(concat (second step-minus-passengers) passengers) 
			(last step-minus-passengers)]
		(if (= 0 (.indexOf step (find-self step)))
			[(first step-minus-passengers) 
				(second step-minus-passengers) 
				(concat (last step-minus-passengers) passengers)]
			[(concat (first step-minus-passengers) passengers) 
				(second step-minus-passengers) 
				(last step-minus-passengers)])]))

(defn who-is-with-you [step]
	(let [stuff-to-bring (remove #(= :you %) (find-self step))]
		(conj (map (fn[x] [:you x]) stuff-to-bring) [:you])))

(defn find-next-steps[step]
	(for [passengers (who-is-with-you step)]
		(bring-across step passengers)))


(defn find-possible-moves [plan]
	(let [last-step (last plan)
		proposed-steps (find-next-steps last-step)]
		(filter 
			(fn [steps]
				(every? identity (map (partial valid-next-step? plan) steps))) 
			proposed-steps)))

(defn generate-next-plans [plan]
	(let [new-moves (find-possible-moves plan)]
		(if(empty? new-moves)
			[]
			(map #(concat plan %) new-moves))))

(defn plan-successful? [plan]
	(println [(map set (last plan))])
	(= (map set (last plan))
		[#{} #{:boat} #{:you :goose :corn :fox}]))

(defn generate-successful-plans [plans]
	(loop [working-plans plans
		successful-plans []]
		(if (empty? working-plans)
			(do (println "working empty") successful-plans)
			(let [this-plan (first working-plans)]
				(println "working not empty")
				(if (plan-successful? this-plan)
					(do (println "found success")
					(recur (rest working-plans) 
						(conj successful-plans this-plan)))
					(do (println "found fail")
					(recur (concat (rest working-plans) (generate-next-plans this-plan))
						successful-plans)))
				))))



(defn generate-plans[]
	(generate-successful-plans[start-pos]))

(defn river-crossing-plan []
	(first (generate-plans)))
