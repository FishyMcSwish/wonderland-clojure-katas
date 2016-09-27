(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-find-winning-card
  (testing "queens are higher rank than jacks"
    (is (= [:queen :spades] (find-winning-card [:queen :spades] [:jack :spades]))))
  (testing "kings are higher rank than queens"
    (is (= [:king :diamonds] (find-winning-card [:king :diamonds] [:queen :diamonds]))))
  (testing "aces are higher rank than kings"
    (is (= [:ace :spades] (find-winning-card [:king :diamonds] [:ace :spades]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= [:ace :clubs] (find-winning-card [:ace :clubs] [:ace :spades]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= [5 :diamonds] (find-winning-card [5 :diamonds] [5 :clubs]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= [2 :hearts] (find-winning-card [2 :hearts] [2 :diamonds])))))

(deftest test-play-round
  (testing "player 1 should win our test setup and take player 2's card"
    (is (= (play-round [[:king :spades] [2 :hearts]] [[10 :clubs] [5 :diamonds]])
           [[[2 :hearts] [:king :spades] [10 :clubs]] [[5 :diamonds]]])))
  (testing "player 2 should win our test setup and take player 2's card"
    (is (= (play-round [[:king :spades] [2 :hearts]] [[:king :hearts] [5 :diamonds]])
           [[[2 :hearts]] [[5 :diamonds] [:king :hearts] [:king :spades]]]))))

(deftest test-play-game
  (testing "player2 loses when they run out of cards"
    (is (= :player1 (play-game [[:king :hearts] [:ace :spades]]
                               [[10 :diamonds] [2 :clubs]]))))
  (testing "player1 loses when they run out of cards"
      (is (= :player2 (play-game [[:king :hearts] [:ace :spades]]
                                 [[:ace :diamonds] [2 :clubs]])))))