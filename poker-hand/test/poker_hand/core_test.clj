(ns poker-hand.core-test
  (:require [clojure.test :refer :all]
            [poker-hand.core :refer :all]))

(def player1 "Player 1")
(def player2 "Player 2")
(def player3 "Player 3")

(deftest reading-poker-hand-test
  (testing "reading a poker card"
    (is (= (parse-card "AS") {:suit :spade, :rank :ace}))
    (is (= (parse-card "TH") {:suit :heart, :rank :10}))
    (is (= (parse-card "3C") {:suit :club, :rank :3}))
    (is (= (parse-card "QD") {:suit :diamond, :rank :queen})))

  (testing "reading a poker hand"
    (is (= (parse-cards "AS TH 3C QD 8C")
           [{:suit :spade, :rank :ace},
            {:suit :heart, :rank :10},
            {:suit :club, :rank :3},
            {:suit :diamond, :rank :queen},
            {:suit :club, :rank :8}]))))

(deftest winning-hand-test

  (testing "incorrect poker game !"
    (is (thrown-with-msg? Exception
                          #"Some players have the same cards !"
                          (winning-hand {player1 "AS QD 3D TD 5S", player2 "AS QH 3C TH 9C"}))))

  (testing "Winning hand with highest card"
    (is (= (winning-hand {
                          player1 "AS QD 3D TD 5S",
                          player2 "AC QH 3C TH 9C",
                          player3 "AD QC 5H JH 7C"
                          })
           "Player 3 has win the game with highest card :jack of :heart")))

  (testing "Winning hand with pair"
    (is (= (winning-hand {player1 "2S QD 5D 3D 5S", player2 "AC QH 3C TH 9C"})
           "Player 1 has win the game with pair of :5")))
  )
