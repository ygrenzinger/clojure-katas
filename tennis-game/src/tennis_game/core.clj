(ns tennis-game.core
  (:gen-class))

(defn translate-points [points]
  (case points
    0 "love"
    1 "fifteen"
    2 "thirty"
    3 "forty"))

(defn translate [points-by-player]
  (let [points (vals points-by-player)
        max (apply max points)
        min (apply min points)
        winning-player (key (first (filter #(= max (val %)) points-by-player)))]
    (cond
      (and (> max 3) (= 2 (- max min))) (str winning-player " has win the game")
      (and (> max 3) (> max min)) (str winning-player " has advantage")
      (and (> max 3) (= max min)) "Deuce"
      :else (str (translate-points max) " - " (translate-points min) " for " winning-player))
    ))

(defn points-by-player [player1 player2 grouped-by-player]
  {player1 (count (get grouped-by-player player1 [])),
   player2 (count (get grouped-by-player player2 []))})

(defn points-win-by [player1 player2 players]
  (if (empty? players)
    "love - love"
    (let [grouped-by-player (group-by identity players)
          points-by-player (points-by-player player1 player2 grouped-by-player)]
      (translate points-by-player))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
