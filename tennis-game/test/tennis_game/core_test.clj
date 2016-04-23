(ns tennis-game.core-test
  (:require [clojure.test :refer :all]
            [tennis-game.core :refer :all]))

(def player1 "Player 1")
(def player2 "Player 2")

(deftest TennisGame
  (testing "No player has win a single points"
    (is (= (points-win-by player1 player2 []) "love - love")))
  (testing "Player 1 has win 1 point"
    (is (= (points-win-by player1 player2 [player1]) "fifteen - love for Player 1")))
  (testing "Player 1 has win 2 points"
    (is (= (points-win-by player1 player2 [player1, player1]) "thirty - love for Player 1")))
  (testing "Player 1 has win 3 points"
    (is (= (points-win-by player1 player2 [player1, player1, player1]) "forty - love for Player 1")))
  (testing "Player 2 has win 3 points and Player 1 has win 2 point"
    (is (= (points-win-by player1 player2 [player1, player1, player2, player2, player2]) "forty - thirty for Player 2")))
  (testing "Advantage"
    (is (= (points-win-by player1 player2 [player1, player2, player1, player2, player1, player2, player2]) "Player 2 has advantage"))
    (is (= (points-win-by player1 player2 [player1, player2, player1, player2, player1, player2, player1]) "Player 1 has advantage")))
  (testing "Deuce"
    (is (= (points-win-by player1 player2 [player1, player2, player1, player2, player1, player2, player1, player2]) "Deuce"))))
