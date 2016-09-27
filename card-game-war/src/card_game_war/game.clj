(ns card-game-war.game)

(def suits [:spades :clubs :diamonds :hearts])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [rank suit]))

(defn find-winning-card [player1-card player2-card]
  (->> [player1-card player2-card]
       (sort-by (juxt #(.indexOf ranks (first %))
                      #(.indexOf suits (second %))))
       (last)))

(defn play-round [player1-cards player2-cards]
  (let [card1 (first player1-cards) card2 (first player2-cards)]
    (if (= card1 (find-winning-card card1 card2))
      [(concat (rest player1-cards) [card1 card2]) (rest player2-cards)]
      [(rest player1-cards) (concat (rest player2-cards) [card2 card1])])))

(defn play-game [player1-cards player2-cards]
  (loop [[player1-cards player2-cards] (play-round player1-cards player2-cards)]
    (if (= 0 (count player2-cards))
      :player1
      (if (= 0 (count player1-cards))
        :player2
        (recur (play-round player1-cards player2-cards))))))

(defn war! []
  (let [stacks (partition 26 (shuffle cards))]
    (play-game (first stacks) (second stacks))))
