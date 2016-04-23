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

(defn parse-card [value]
  {:suit (get suit-char (nth value 1)),
   :rank (get rank-char (nth value 0))})

(defn parse-cards [value]
  (map parse-card (clojure.string/split value #" ")))

(defn parse-player-hand [player-hand]
  {:name (key player-hand) :cards (parse-cards (val player-hand))})

(defn associate-card-to-player [player-hand]
  (zipmap (:cards player-hand) (repeat (:name player-hand))))

(defn parse-hands [hands]
  (let [nb-of-players (count hands)
        parsed-hands (map parse-player-hand hands)
        cards-associated-to-player (into {} (apply concat (map associate-card-to-player parsed-hands)))]
    (if (not= (* 5 nb-of-players) (count cards-associated-to-player))
      (throw (Exception. "Some players have the same cards !"))
      cards-associated-to-player)
    )
  )

(defn highest-card [hands]
  (let [grouped-by-rank (group-by #(:rank (key %)) hands)
        filtered-for-one-card-by-rank (filter #(= 1 (count (val %))) grouped-by-rank)]
    (if (empty? filtered-for-one-card-by-rank)
      nil
      (let [[_ [max]] (apply max-key #(rank-values (key %)) filtered-for-one-card-by-rank)]
        {:name (val max) :card (key max)}))))

(defn count-rank [cards]
  (frequencies (map #(:rank (nth % 0)) cards)))

(defn find-multiple-rank [grouped-by-player multiple]
  (map (fn [hand] [(key hand) (filter #(= multiple (second %)) (count-rank (val hand)))]) grouped-by-player))

(defn winning-multiple-rank [hands multiple]
  (let [grouped-by-player (group-by val hands)
        hands-with-pair (filter #(not-empty (second %)) (find-multiple-rank grouped-by-player multiple))]
    (if (empty? hands-with-pair)
      []
      (let [pair-by-player (into {} (map (fn [[k v]] [k (first (first v))]) hands-with-pair))
            pair-sorted-by-card-values (reverse (sort (group-by #(rank-values (val %)) pair-by-player)))]
        (second (first pair-sorted-by-card-values))))))

(defn winning-hand [hands]
  (let [hands-parsed (parse-hands hands)]
    (let [winning-pairs (winning-multiple-rank hands-parsed 2)
          winning-card (highest-card hands-parsed)]
      (cond
        (= 1 (count winning-pairs)) (str (first (first winning-pairs)) " has win the game with pair of " (second (first winning-pairs)))
        (not (nil? winning-card)) (str (:name winning-card) " has win the game with highest card " (:rank (:card winning-card)) " of " (:suit (:card winning-card)))
        :else "Draw")
      )
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
