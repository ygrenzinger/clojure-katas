(ns tennis-game.core
  (:gen-class))

(defn point-translation [point]
  (get {0 "love", 1 "fifteen", 2 "thirty", 3 "forty"} point))

(defn translate [points-by-player]
  (println " points-by-player : " points-by-player)
  (let [points (vals points-by-player)
        max (apply max points)
        min (apply min points)
        winning-player (key (first (filter #(= max (val %)) points-by-player)))]
    (cond
      (= min max 0) "love - love"
      (and (> max 3) (= 2 (- max min))) (str winning-player " has win the game")
      (and (> max 3) (> max min)) (str winning-player " has advantage")
      (and (> max 3) (= max min)) "Deuce"
      :else (str (point-translation max) " - " (point-translation min) " for " winning-player))
    ))

(defn points-win-by [player1 player2 players-who-scored]
  (let [points-by-player {player1 0, player2 0}
        points-by-player (into points-by-player (frequencies players-who-scored))]
    (translate points-by-player)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
