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

(deftest special-hands-test
  (testing "pair ?"
    (is (= {:rank :8, :remaining-cards [{:suit :spade, :rank :ace} {:suit :club, :rank :3} {:suit :diamond, :rank :queen}]}
           (one-pair? (parse-cards "AS 8H 3C QD 8C")))))
  (testing "double pair ?"
    (is (= {:rank [:ace :8], :remaining-cards [{:suit :club, :rank :3}]} (double-pair? (parse-cards "AS 8H 3C 8D AC")))))
  (testing "three of a kind ?"
    (is (= {:rank :3, :remaining-cards [{:suit :spade, :rank :ace} {:suit :club, :rank :8}]} (three-of-a-kind? (parse-cards "AS 3H 3C 3D 8C")))))
  (testing "four of a kind ?"
    (is (= {:rank :ace, :remaining-cards [{:suit :club, :rank :8}]} (four-of-a-kind? (parse-cards "AS AH AC AD 8C")))))
  (testing "full"
    (is (= [:2 :queen] (full? (parse-cards "QS 2H QC 2D 2C")))))
  (testing "flush ?"
    (is (= {:suit :diamond, :rank :king} (flush? (parse-cards "KD QD 3D 2D 8D")))))
  (testing "straight ?"
    (is (= {:suit :heart, :rank :6}) (straight? (parse-cards "2S 6H 3D 4H 5D")))
    (is (= nil) (straight? (parse-cards "2S 6H 8D 4H 5D"))))
  (testing "straight flush"
    (is (= {:suit :heart, :rank :jack}) (straight-flush? (parse-cards "JD 9D TD 7D 8D")))))

(deftest winning-hand-test
  (testing "winning straight flush ?"
    (is (= nil (straight-flush-winner? (parse-hands {player1 "AD KD QD JD TS", player2 "AD KD QD JD TH"}))))
    (is (= "Player 1 wins with a straight flush of :diamond at rank :ace"
           (straight-flush-winner? (parse-hands {player1 "AD KD QD JD TD", player2 "AD KD QD JD TH"}))))
    (is (= "Winning with straight flush : Player 2 with :ace of :heart"
           (straight-flush-winner? (parse-hands {player1 "9D KD QD JD TD", player2 "AH KH QH JH TH"}))))
    (is (= "Winning with straight flush : Player 1 with :ace of :diamond, Player 2 with :ace of :heart"
           (straight-flush-winner? (parse-hands {player1 "AD KD QD JD TD", player2 "AH KH QH JH TH"})))))

  (testing "winning flush ?"
    (is (= nil (flush-winner? (parse-hands {player1 "AD KD 5D JD TS", player2 "AD KD 5D JD TH"}))))
    (is (= "Player 1 wins with a flush of :diamond at rank :ace"
           (flush-winner? (parse-hands {player1 "AD KD 5D JD TD", player2 "AD KD 5D JD TH"}))))
    (is (= "Winning with flush : Player 2 with :king of :heart"
           (flush-winner? (parse-hands {player1 "9D 7D 5D JD TD", player2 "8H KH 5H JH TH"}))))
    (is (= "Winning with flush : Player 1 with :king of :diamond, Player 2 with :king of :heart"
           (flush-winner? (parse-hands {player1 "9D KD 5D JD TD", player2 "8H KH 5H JH TH"})))))

  (testing "winning straight ?"
    (is (= nil (straight-winner? (parse-hands {player1 "AD KS QD JD 5S", player2 "AD KD 6D JD TC"}))))
    (is (= "Player 1 wins with a straight of :diamond at rank :ace"
           (straight-winner? (parse-hands {player1 "AD KD QD JD TD", player2 "5D KD QD JD TH"}))))
    (is (= "Winning with straight : Player 2 with :ace of :heart"
           (straight-winner? (parse-hands {player1 "9C KD QH JD TS", player2 "AH KS QD JS TD"}))))
    (is (= "Winning with straight : Player 1 with :ace of :spade, Player 2 with :ace of :club"
           (straight-winner? (parse-hands {player1 "AS KS QD JH TC", player2 "AC KS QC JC TD"})))))

  (testing "winning four of a kind ?"
    (is (= nil (four-of-a-kind-winner? (parse-hands {player1 "4D AS AC AH 5S", player2 "4D AS AC AH 7H"}))))
    (is (= "Player 1 wins with a four of kind of :ace"
           (four-of-a-kind-winner? (parse-hands {player1 "AD AS AC AH 5S", player2 "AD AS 4C AH 7H"}))))
    (is (= "Player 2 wins with a four of kind of :queen"
           (four-of-a-kind-winner? (parse-hands {player1 "8D 8S 8C 8H 5S", player2 "QD QS QC QH 7H"}))))
    (is (= "Player 2 wins with a four of kind of :ace and highest card :7 of :heart"
           (four-of-a-kind-winner? (parse-hands {player1 "AD AS AC AH 5S", player2 "AD AS AC AH 7H"}))))
    (is (= "Player 1 and Player 2 win with a four of kind of :ace"
           (four-of-a-kind-winner? (parse-hands {player1 "AD AS AC AH 7S", player2 "AD AS AC AH 7H"})))))

  (testing "winning three of a kind ?"
    (is (= nil (three-of-a-kind-winner? (parse-hands {player1 "4D AS 2C AH 5S", player2 "4D AS AC 2H 7H"}))))
    (is (= "Player 1 wins with a three of kind of :ace"
           (three-of-a-kind-winner? (parse-hands {player1 "AD 2S AC AH 5S", player2 "AD 2S 4C AH 7H"}))))
    (is (= "Player 2 wins with a three of kind of :queen"
           (three-of-a-kind-winner? (parse-hands {player1 "8D 2S 8C 8H 5S", player2 "QD 2S QC QH 7H"}))))
    (is (= "Player 2 wins with a three of kind of :ace and highest card :7 of :heart"
           (three-of-a-kind-winner? (parse-hands {player1 "AD AS 2C AH 5S", player2 "AD AS 2C AH 7H"}))))
    (is (= "Player 1 and Player 2 win with a three of kind of :ace"
           (three-of-a-kind-winner? (parse-hands {player1 "AD AS 2C AH 7S", player2 "AD AS 2D AH 7H"})))))

  (testing "winning pair ?"
    (is (= nil (pair-winner? (parse-hands {player1 "4D 6S 2C AH 5S", player2 "4D 8S AC 2H 7H"}))))
    (is (= "Player 1 wins with a pair of :ace"
           (pair-winner? (parse-hands {player1 "AD 2S AC 7H 5S", player2 "AD 3S 4C 8C 7H"}))))
    (is (= "Player 2 wins with a pair of :queen"
           (pair-winner? (parse-hands {player1 "8D 2S 8C 3H 5S", player2 "QD 2S QC 5H 7H"}))))
    (is (= "Player 2 wins with a pair of :ace and highest card :7 of :heart"
           (pair-winner? (parse-hands {player1 "AD AS 2D 3H 5S", player2 "AD AS 2C 3D 7H"}))))
    (is (= "Player 1 and Player 2 win with a pair of :ace"
           (pair-winner? (parse-hands {player1 "AD AS 2C 5H 7S", player2 "AD AS 2D 5D 7H"})))))

  (testing "winning full house ?"
    (is (= nil (full-winner? (parse-hands {player1 "AS AC AD 2D 3D", player2 "AS 2C AD 3S 3D"}))))
    (is (= "Player 1 wins with a full at :ace and :3"
           (full-winner? (parse-hands {player1 "AS AC AD 3D 3D", player2 "AS 2C AD 3S 3D"}))))
    (is (= "Player 1 wins with a full of tree :ace and two :3"
           (full-winner? (parse-hands {player1 "AS AC AD 3D 3D", player2 "QS QC QD 8S 8D"}))))
    (is (= "Player 2 wins with a full of tree :ace and two :8"
           (full-winner? (parse-hands {player1 "AS AC AD 3H 3D", player2 "AS AC AD 8S 8D"}))))
    (is (= "Player 1 and Player 2 win with a full of three :ace and two :3"
           (full-winner? (parse-hands {player1 "AS AC AD 3H 3D", player2 "AS AC AD 3S 3D"})))))

  (testing "winning with two pair ?"
    (is (= nil (double-pair-winner? (parse-hands {player1 "AS 6C AD 2D 3D", player2 "AS 2C AD 8S 3D"}))))
    (is (= "Player 1 wins with double pair of :ace and :3"
           (double-pair-winner? (parse-hands {player1 "AS 7C AD 3S 3D", player2 "AS 2C AD 4S 3D"}))))
    (is (= "Player 2 wins with double pair of :king and :3"
           (double-pair-winner? (parse-hands {player1 "QS 7C QD 3S 3D", player2 "3S 2C 3D KS KD"}))))
    (is (= "Player 2 wins with double pair of :queen and :5"
           (double-pair-winner? (parse-hands {player1 "QS 7C QD 3S 3D", player2 "5S 2C 5D QS QD"}))))
    (is (= "Player 1 wins with a double pair of :queen and :5 and highest card :7 of :club"
           (double-pair-winner? (parse-hands {player1 "QS 7C QD 5C 5H", player2 "5S 2C 5D QH QC"}))))
    (is (= "Player 1 and Player 2 win with a double pair of :queen and :5"
           (double-pair-winner? (parse-hands {player1 "QS 7C QD 5C 5H", player2 "5S 7S 5D QH QC"}))))))



(deftest winning-test

  (testing "Winning with highest card"
    (is (= (winner? {player1 "AS QD 3D TD 5S",
                     player2 "AC QH 3C TH 9C",
                     player3 "AD QC 5H JH 7C"})
           "Player 3 wins with highest card :jack of :heart")))

  (testing "Winning with pair"
    (is (= (winner? {player1 "2S QD 5D 3D 5S", player2 "AC QH 3C TH 9C"})
           "Player 1 wins with a pair of :5")))

  (testing "Winning with double pair"
    (is (= (winner? {player1 "2S QD 5D 3D 5S", player2 "AC 3H TC TH 3C"})
           "Player 2 wins with double pair of :10 and :3")))

  (testing "Winning with three of a kind"
    (is (= (winner? {player1 "3H QD 5D 3D 3S", player2 "AC 5H TC TH 5C"})
           "Player 1 wins with a three of kind of :3")))

  (testing "Winning with straight"
    (is (= (winner? {player1 "3H QD 5D 3D 3S", player2 "3C 5H 4C 7H 6C"})
           "Player 2 wins with a straight of :heart at rank :7")))

  (testing "Winning with flush"
    (is (= (winner? {player1 "3S QS 5S 8S TS", player2 "3C 5H 4C 7H 6C"})
           "Player 1 wins with a flush of :spade at rank :queen")))

  (testing "Winning with full house"
    (is (= (winner? {player1 "3C QS 5S 8D TH", player2 "3S 3H QC QH QS"})
           "Player 2 wins with a full at :queen and :3")))

  (testing "Winning with a four of a kind"
    (is (= (winner? {player1 "5C 5S 7S 5D 5H", player2 "3S 3H QC QH QS"})
           "Player 1 wins with a four of kind of :5")))

  (testing "Winning with a straight flush"
    (is (= (winner? {player1 "5C 5S 7S 5D 5H", player2 "6H 7H TH 9H 8H"})
           "Player 2 wins with a straight flush of :heart at rank :10")))

  )
