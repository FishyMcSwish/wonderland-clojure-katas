(ns fox-goose-bag-of-corn.puzzle-test
  (:require [clojure.test :refer :all]
            [fox-goose-bag-of-corn.puzzle :refer :all]
            [clojure.set]))

(defn validate-move [step1 step2]
  (testing "only you and another thing can move"
    (let [diff1 (clojure.set/difference step1 step2)
          diff2 (clojure.set/difference step2 step1)
          diffs (concat diff1 diff2)
          diff-num (count diffs)]
      (is (> 3 diff-num))
      (when (pos? diff-num)
        (is (contains? (set diffs) :you)))
      step2)))

(deftest test-river-crossing-plan
  (let [crossing-plan (map (partial map set) (river-crossing-plan))]
    (testing "you begin with the fox, goose and corn on one side of the river"
      (is (= [#{:you :fox :goose :corn} #{:boat} #{}]
             (first crossing-plan))))
    (testing "you end with the fox, goose and corn on one side of the river"
      (is (= [#{} #{:boat} #{:you :fox :goose :corn}]
             (last crossing-plan))))
    (testing "things are safe"
      (let [left-bank (map first crossing-plan)
            right-bank (map last crossing-plan)]
        (testing "the fox and the goose should never be left alone together"
          (is (empty?
               (filter #(= % #{:fox :goose}) (concat left-bank right-bank)))))
        (testing "the goose and the corn should never be left alone together"
          (is (empty?
               (filter #(= % #{:goose :corn}) (concat left-bank right-bank)))))))
    (testing "The boat can carry only you plus one other"
      (let [boat-positions (map second crossing-plan)]
        (is (empty?
             (filter #(> (count %) 3) boat-positions)))))
    (testing "moves are valid"
      (let [left-moves (map first crossing-plan)
            middle-moves (map second crossing-plan)
            right-moves (map last crossing-plan)]
        (reduce validate-move left-moves)
        (reduce validate-move middle-moves)
        (reduce validate-move right-moves )))))

(def complete-plan [[[:fox :goose :corn :you] [:boat] []]
        [[:fox :corn] [:boat :you :goose] []]
        [[:fox :corn] [:boat] [:you :goose]]
        [[:fox :corn] [:boat :you] [:goose]]
        [[:fox :corn :you] [:boat] [:goose]]
        [[:fox] [:boat :you :corn] [:goose]]
        [[:fox] [:boat] [:goose :you :corn]]
        [[:fox] [:boat :you :goose] [:corn]]
        [[:fox :you :goose] [:boat] [:corn]]
        [[:goose] [:boat :you :fox] [:corn]]
        [[:goose] [:boat] [:corn :you :fox]]
        [[:goose] [:boat :you] [:corn :fox]]
        [[:goose :you] [:boat] [:corn :fox]]
        [[] [:boat :you :goose] [:corn :fox]]
        [[] [:boat] [:you :goose :corn :fox]]])

(def partial-plan [[[:fox :goose :corn :you] [:boat] []]
        [[:fox :corn] [:boat :you :goose] []]
        [[:fox :corn] [:boat] [:you :goose]]
        [[:fox :corn] [:boat :you] [:goose]]
        [[:fox :corn :you] [:boat] [:goose]]
        [[:fox] [:boat :you :corn] [:goose]]
        [[:fox] [:boat] [:goose :you :corn]]
        [[:fox] [:boat :you :goose] [:corn]]
        [[:fox :you :goose] [:boat] [:corn]]
        [[:goose] [:boat :you :fox] [:corn]]])

(deftest test-repeating-positions?
  (testing "repeating-positions? returns true if plan already has the new position"
    (is (= true (repeating-positions? [[:goose] [:boat] [:corn :you :fox]] complete-plan))))
  (testing "repeating-positions? returns nil if the plan does not include the new position"
    (is (nil? (repeating-positions? [[:fox :goose] [:boat :you] [:corn]] complete-plan)))))

(deftest test-is-safe?
  (testing "it's not safe leaving a fox with a goose"
    (is (false? (is-safe? [[:fox :goose] [:boat :you :corn] []]))))
  (testing "it's not safe to leave a goose with corn"
    (is (false? (is-safe? [[[] [:boat :fox :you] [:corn :goose]]]))))
  (testing "it's not safe to overload the boat"
    (is (false? (is-safe? [[] [:boat :you :goose :fox] [:corn]]))))
  (testing "you need a boat in the water"
    (is (false? (is-safe? [[] [] [:boat :you :goose :fox :corn]]))))
  (testing "some things are safe"
    (is (true? (is-safe? [[:fox :you :goose] [:boat] [:corn]]))))
  (testing "leaving everyone else together is safe"
    (is (true? (is-safe? [[:goose :corn :fox] [:boat :you] []]))))
  (testing "no intruders"
    (is (false? (is-safe? [[:goose :corn :fox :you] [:boat] [:intruder]]))))
  (testing "you can't eliminate anybody"
    (is (false? (is-safe? [[:goose] [:boat :you] [:fox]])))))

(deftest test-valid-next-step?
  (testing "say no to dupes"
    (is (false? (valid-next-step? partial-plan [[:fox :you :goose] [:boat] [:corn]]))))
  (testing "say no to the unsafe"
    (is (false? (valid-next-step? partial-plan [[:fox :you] [:boat] [:goose :corn]])))))

