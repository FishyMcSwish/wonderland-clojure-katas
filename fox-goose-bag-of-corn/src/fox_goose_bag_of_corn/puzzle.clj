(ns fox-goose-bag-of-corn.puzzle)


(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn repeating-positions? [newstep plan]
	"Determines if this step repeats a previous step"
	(some #(= % (map set newstep)) (map #(map set %) plan)))

;todo: break this bad boy up
(defn is-safe? [newstep]
	"For a given proposed step (e.g. [[:fox :corn:] [:you :boat :goose] []]),
	determines if the step violates any of the rules of the game"
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
	"ensures the given step is both safe and non-repeating"
	(and (is-safe? newstep)
		(not (repeating-positions? newstep plan))))

(defn find-self [step]
	"finds :you, and everyone with you.
	e.g. (find-self [[:goose][:boat] [:you :fox :corn]]) => [:you :fox :corn]"

	(first (filter #(contains? (set %) :you) step)))

(defn bring-across [step passengers]
	"adds two steps to the plan, one to put :you and your new passenger (if needed) in a boat,
	and a second to bring :you and the passenger to the other side"
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

(defn find-possible-passengers [step]
	"finds all possible passengers you could take across the river.
	e.g. (find-possible-passengers [[:goose] [:boat] [:you :fox :corn] =>
	([:you] [:you :fox] [:you :corn])"
	(let [stuff-to-bring (remove #(= :you %) (find-self step))]
		(conj (map (fn[x] [:you x]) stuff-to-bring) [:you])))

(defn find-next-steps[step]
	(for [passengers (find-possible-passengers step)]
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
	(= (map set (last plan))
		[#{} #{:boat} #{:you :goose :corn :fox}]))

(defn generate-successful-plans [plans]
	(loop [working-plans plans
		successful-plans []]
		(if (empty? working-plans)
			successful-plans
			(let [this-plan (first working-plans)]
				(if (plan-successful? this-plan)
					(recur (rest working-plans)
								 (conj successful-plans this-plan))
					(recur (concat (rest working-plans) (generate-next-plans this-plan))
								 successful-plans))))))

(defn generate-plans[]
	"returns all successful river crossing plans given our defined starting position"
	(generate-successful-plans[start-pos]))

(defn river-crossing-plan []
	"returns only the first successful plan"
	(first (generate-plans)))
