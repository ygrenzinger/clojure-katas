(ns poker-hand.core
  (:gen-class))

(def suit-char {\S :spade, \C :club, \H :heart, \D :diamond})
(def rank-char {
                \2 :2,
                \3 :3,
                \4 :4,
                \5 :5,
                \6 :6,
                \7 :7,
                \8 :8,
                \9 :9,
                \T :10,
                \J :jack,
                \Q :queen,
                \K :king,
                \A :ace})

(def rank-values {
                  :2     0,
                  :3     1,
                  :4     2,
                  :5     3,
                  :6     4,
                  :7     5,
                  :8     6,
                  :9     7,
                  :10    8,
                  :jack  9,
                  :queen 10,
                  :king  11,
                  :ace   12})

;parsing functions
(defn parse-card [value]
  {:suit (get suit-char (nth value 1)),
   :rank (get rank-char (nth value 0))})

(defn parse-cards [value]
  (map parse-card (clojure.string/split value #" ")))

(defn parse-hands [hands]
  (into {} (map (fn [[player hands-not-parsed]] [player (parse-cards hands-not-parsed)]) hands)))

;Winning figure functions
(defn associate-cards-to-player [hands-parsed]
  (into {} (apply concat (map (fn [hand] (zipmap (val hand) (repeat (key hand)))) hands-parsed))))

(defn highest-player-card? [cards-associated-to-player]
  (let [grouped-by-rank (group-by #(:rank (key %)) cards-associated-to-player)
        filtered-for-one-card-by-rank (filter #(= 1 (count (val %))) grouped-by-rank)]
    (if (empty? filtered-for-one-card-by-rank)
      nil
      (let [[_ [max]] (apply max-key #(rank-values (key %)) filtered-for-one-card-by-rank)]
        {:name (val max) :card (key max)}))))

(defn highest-player-card-winner? [hands]
  (let [highest-player-card (highest-player-card? (associate-cards-to-player hands))]
    (if (nil? highest-player-card)
      "Draw"
      (str (:name highest-player-card) " wins with highest card " (:rank (:card highest-player-card)) " of " (:suit (:card highest-player-card))))))

(defn count-rank [cards]
  (frequencies (map #(:rank %) cards)))

(def memoized-count-rank (memoize count-rank))

(defn sort-by-rank-decreasing [cards]
  (reverse (sort-by #(rank-values (:rank %)) cards)))

(defn find-multiple-rank [count-by-rank multiple]
  (filter #(= multiple (val %)) count-by-rank))

(defn multiple-of-a-kind? [cards multiple]
  (let [counted-rank (memoized-count-rank cards)
        multiple-of-a-kind (find-multiple-rank counted-rank multiple)]
    (if-not (empty? multiple-of-a-kind)
      (let [[[rank _]] multiple-of-a-kind]
        {:rank rank, :remaining-cards (filter #(not= rank (:rank %)) cards)})
      nil)))

(defn one-pair? [cards]
  (multiple-of-a-kind? cards 2))

(defn three-of-a-kind? [cards]
  (multiple-of-a-kind? cards 3))

(defn four-of-a-kind? [cards]
  (multiple-of-a-kind? cards 4))

(defn double-pair? [cards]
  (let [counted-rank (memoized-count-rank cards)
        pairs (find-multiple-rank counted-rank 2)]
    (if (= 2 (count pairs))
      (let [rank (vec (reverse (sort-by #(rank-values %) (map #(first %) pairs))))]
        {:rank rank, :remaining-cards (filter #(not (some #{(:rank %)} rank)) cards)})
      nil)))

(defn full? [cards]
  (let [pair (one-pair? cards)
        three-of-a-kind (three-of-a-kind? cards)]
    (if (and three-of-a-kind pair)
      [(:rank three-of-a-kind) (:rank pair)]
      nil)))

(defn flush? [cards]
  (if (apply = (map #(:suit %) cards))
    (first (sort-by-rank-decreasing cards))
    nil))

(defn straight? [cards]
  (let [sorted-cards (sort-by-rank-decreasing cards)
        highest-card (first sorted-cards)
        highest-value (rank-values (:rank highest-card))]
    (if (= (take 5 (iterate dec highest-value)) (map #(rank-values (:rank %)) sorted-cards))
      highest-card
      nil)))

(defn straight-flush? [cards]
  (let [flush-card (flush? cards)
        straight-card (straight? cards)]
    (if (= flush-card straight-card)
      flush-card
      nil)))

(defn filter-special-hand [hands special-categorie?]
  (let [keep-only-found-special-categorie #(not (nil? (second %)))]
    (filter keep-only-found-special-categorie (map (fn [[player hand]] [player (special-categorie? hand)]) hands))))

(defn winning-with-full-hand-figure? [special-categorie? figure hands]
  (let [having-special-hand (filter-special-hand hands special-categorie?)]
    (cond
      (> (count having-special-hand) 1) (let [grouped-by-rank (group-by #(rank-values (:rank (second %))) having-special-hand)
                                              highest-ranks (apply max-key key grouped-by-rank)]
                                          (str "Winning with " figure " : " (apply str (interpose ", " (map #(str (first %) " with " (:rank (second %)) " of " (:suit (second %))) (val highest-ranks))))))
      (= 1 (count having-special-hand)) (let [[[player card]] having-special-hand] (str player " wins with a " figure " of " (:suit card) " at rank " (:rank card)))
      :else nil)))

(defn flush-winner? [hands]
  (winning-with-full-hand-figure? flush? "flush" hands))

(defn straight-winner? [hands]
  (winning-with-full-hand-figure? straight? "straight" hands))

(defn straight-flush-winner? [hands]
  (winning-with-full-hand-figure? straight-flush? "straight flush" hands))

(defn multiple-of-a-kind-winner? [special-categorie? figure hands]
  (let [having-special-hand (filter-special-hand hands special-categorie?)]
    (cond
      (= 1 (count having-special-hand)) (let [[[player result]] having-special-hand] (str player " wins with a " figure " of " (:rank result)))
      (> (count having-special-hand) 1) (let [grouped-by-rank (group-by #(rank-values (:rank (second %))) having-special-hand)
                                              highest-ranks (apply max-key key grouped-by-rank)
                                              winners (second highest-ranks)]
                                          (if (= 1 (count winners))
                                            (str (first (first winners)) " wins with a " figure " of " (:rank (second (first winners))))
                                            (let [remaining-cards (into {} (map (fn [winner] [(first winner) (:remaining-cards (second winner))]) winners))
                                                  highest-cards (highest-player-card? (associate-cards-to-player remaining-cards))]
                                              (if (nil? highest-cards)
                                                (str (apply str (interpose " and " (map #(first %) winners))) " win with a " figure " of " (:rank (second (first winners))))
                                                (str (:name highest-cards) " wins with a " figure " of " (:rank (second (first winners))) " and highest card " (:rank (:card highest-cards)) " of " (:suit (:card highest-cards))))
                                              )
                                            ))
      :else nil)))

(defn four-of-a-kind-winner? [hands]
  (multiple-of-a-kind-winner? four-of-a-kind? "four of kind" hands))

(defn three-of-a-kind-winner? [hands]
  (multiple-of-a-kind-winner? three-of-a-kind? "three of kind" hands))

(defn pair-winner? [hands]
  (multiple-of-a-kind-winner? one-pair? "pair" hands))

(defn full-winner? [hands]
  (let [having-special-hand (filter-special-hand hands full?)]
    (cond
      (= (count having-special-hand) 1) (str (first (first having-special-hand)) " wins with a full at " (first (second (first having-special-hand))) " and " (second (second (first having-special-hand))))
      (> (count having-special-hand) 1) (let [grouped-by-three-of (group-by #(rank-values (first (second %))) having-special-hand)
                                              highest-ranks-three-of (apply max-key key grouped-by-three-of)
                                              winners-three-of (second highest-ranks-three-of)]
                                          (if (= 1 (count winners-three-of))
                                            (let [[[winner [rank-three-of rank-pair-of]]] winners-three-of]
                                              (str winner " wins with a full of tree " rank-three-of " and two " rank-pair-of))
                                            (let [grouped-by-two-of (group-by #(rank-values (second (second %))) winners-three-of)
                                                  highest-ranks-two-of (apply max-key key grouped-by-two-of)
                                                  winners-two-of (second highest-ranks-two-of)]
                                              (if (= 1 (count winners-two-of))
                                                (let [[[winner [rank-three-of rank-pair-of]]] winners-two-of]
                                                  (str winner " wins with a full of tree " rank-three-of " and two " rank-pair-of))
                                                (let [[[_ [rank-three-of rank-pair-of]]] winners-two-of]
                                                  (str (apply str (interpose " and " (map #(first %) winners-two-of))) " win with a full of three " rank-three-of " and two " rank-pair-of)))
                                              )))
      :else nil)))

(defn double-pair-winner? [hands]
  (let [having-special-hand (filter-special-hand hands double-pair?)]
    (cond
      (= (count having-special-hand) 1) (let [[[winner {[pair1 pair2] :rank}]] having-special-hand]
                                          (str winner " wins with double pair of " pair1 " and " pair2))
      (> (count having-special-hand) 1) (let [grouped-by-rank-pair1 (group-by #(rank-values (first (:rank (second %)))) having-special-hand)
                                              highest-ranks-pair1 (apply max-key key grouped-by-rank-pair1)
                                              winners-pair1 (second highest-ranks-pair1)]
                                          (if (= 1 (count winners-pair1))
                                            (let [[[winner {[pair1 pair2] :rank}]] winners-pair1]
                                              (str winner " wins with double pair of " pair1 " and " pair2))
                                            (let [grouped-by-rank-pair2 (group-by #(rank-values (second (:rank (second %)))) winners-pair1)
                                                  highest-ranks-pair2 (apply max-key key grouped-by-rank-pair2)
                                                  winners-pair2 (second highest-ranks-pair2)]
                                              (if (= 1 (count winners-pair2))
                                                (let [[[winner {[pair1 pair2] :rank}]] winners-pair2]
                                                  (str winner " wins with double pair of " pair1 " and " pair2))

                                                (let [remaining-cards (into {} (map (fn [winner] [(first winner) (:remaining-cards (second winner))]) winners-pair2))
                                                      highest-cards (highest-player-card? (associate-cards-to-player remaining-cards))]
                                                  (let [[[_ {[pair1 pair2] :rank}]] winners-pair2]
                                                    (if (nil? highest-cards)
                                                      (str (apply str (interpose " and " (map #(first %) winners-pair2))) " win with a double pair of " pair1 " and " pair2)
                                                      (str (:name highest-cards) " wins with a double pair of " pair1 " and " pair2 " and highest card " (:rank (:card highest-cards)) " of " (:suit (:card highest-cards)))))
                                                  )
                                                )
                                              )))
      :else nil)))

(defn winner? [hands]
  (let [poker-hands-categories-in-winning-order [straight-flush-winner?
                                                 four-of-a-kind-winner?
                                                 full-winner?
                                                 flush-winner?
                                                 straight-winner?
                                                 three-of-a-kind-winner?
                                                 double-pair-winner?
                                                 pair-winner?
                                                 highest-player-card-winner?]
        parsed-hands (parse-hands hands)]
    (first (drop-while #(nil? %) (map (fn [win?] (win? parsed-hands)) poker-hands-categories-in-winning-order)))))

(defn build-all-possible-card-input []
  (for [suit-char (keys suit-char)
        rank-char (keys rank-char)]
    (str rank-char suit-char)))

(defn get-hand-for-players
  "build game with a hand for each given player"
  ([players] (get-hand-for-players players true))
  ([players random]
   (let [possible-cards-input (build-all-possible-card-input)
         cards (if random (shuffle possible-cards-input) possible-cards-input)]
     (loop [pool-of-cards cards
            remaining-players players
            game {}]
       (let [cards-hand (apply str (interpose " " (take 5 pool-of-cards)))
             hand {(first remaining-players), cards-hand}]
         (if (empty? remaining-players)
           game
           (recur (drop 5 pool-of-cards)
                  (rest remaining-players)
                  (into game hand))))))))

(defn play-random-game [players]
  (let [hands (get-hand-for-players players)]
    {:hands hands, :winner (winner? hands)}))

(defn print-game [game]
  (println "Game = " (:hands game))
  (println "Winner = " (:winner game))
  (println ""))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (doall (map print-game (repeatedly 20 #(play-random-game ["Sylvain" "Bruno" "Yannick" "Diego" "Somkiane"])))))
