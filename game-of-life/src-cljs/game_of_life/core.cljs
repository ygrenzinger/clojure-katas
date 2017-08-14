(ns game-of-life.core
  (:require [clojure.string :as string]
            [reagent.core :as r]))

(enable-console-print!)

(def width (r/atom 10))
(def height (r/atom 10))
(def percentageTrigger (r/atom 50))

(defn get-random-state []
  (let [randomPercentage (rand-int 101)]
    (if (>= randomPercentage @percentageTrigger)
      0
      1)))

(defn generate-row []
  (vec (repeatedly @width #(get-random-state))))

(defn generate-grid []
  (vec (repeatedly @height #(generate-row))))

(def grid (r/atom (generate-grid)))

;return 0 if out of grid (there is a dead land out there
(defn get-cell-state [row-idx col-idx]
  (get-in @grid [row-idx col-idx] 0))

(defn get-inverted-cell-state [row-idx col-idx]
  (if (= 0 (get-cell-state row-idx col-idx)) 1 0))

(defn switch-cell-state! [row-idx col-idx]
  (swap! grid assoc-in [row-idx col-idx] (get-inverted-cell-state row-idx col-idx)))

(defn neighbours [row-idx col-idx]
  [[(dec row-idx) (dec col-idx)] [(dec row-idx) (identity col-idx) ] [(dec row-idx) (inc col-idx)]
   [row-idx (dec col-idx)] [row-idx (inc col-idx)]
   [(inc row-idx) (dec col-idx)] [(inc row-idx) col-idx] [(inc row-idx) (inc col-idx)]])

(defn count-alive-neighbours [row-idx col-idx]
  (println "row index " row-idx)

  (reduce + (map #(apply get-cell-state %) (neighbours row-idx col-idx))))

(defn alive-cell? [row-idx col-idx]
  (= 1 (get-cell-state row-idx col-idx)))

(defn next-cell-state [row-idx col-idx]
  (let [alive? (alive-cell? row-idx col-idx)
        alive-neighbours (count-alive-neighbours row-idx col-idx)]
    (if alive?
      (if (and (> alive-neighbours 1) (< alive-neighbours 4)) 1 0)
      (if (= alive-neighbours 3) 1 0))))

(defn next-row [row-idx row]
  (vec (map-indexed (fn [col-idx _] (next-cell-state row-idx col-idx)) row)))

(defn next-grid []
  (vec (map-indexed
         (fn [row-idx row] (next-row row-idx row))
         @grid)))

(defn draw-cell [row-idx col-idx cell-state]
  (let [color (if (= 1 cell-state) "black" "white")]
    ^{:key (str row-idx col-idx)} [:rect
                                   {:on-click #(switch-cell-state! row-idx col-idx)
                                    :x (* 10 col-idx)
                                    :y (* 10 row-idx)
                                    :width 10
                                    :height 10
                                    :style {:fill color}}]))

(defn draw-row [row-idx row]
  (map-indexed (fn [idx cell] (draw-cell row-idx idx cell)) row))

(defn draw-grid []
  (map-indexed draw-row @grid))

(def refreshing-grid? (r/atom false))

(defn refresh-grid []
  (if @refreshing-grid?
    (reset! grid (next-grid))))

(defn refreshing-grid []
  (do
    (refresh-grid)
    (if @refreshing-grid?
      (js/setTimeout #(refreshing-grid) 500))))

(defn switch-refreshing []
  (if @refreshing-grid?
    (reset! refreshing-grid? false)
    (do
      (reset! refreshing-grid? true)
      (refreshing-grid))))


(defn game-of-life []
  [:div {:class "container"}
   [:h1 "Game of life"]
   [:div {:class "form-inline"}
    [:div {:class "form-group"}
     [:label {:for "width"} "width"]
     [:input {:id        "width"
              :type      "number"
              :value     @width
              :on-change #(reset! width (-> % .-target .-value))}]]

    [:div {:class "form-group"}
     [:label {:for "percent"} "height"]
     [:input {:id        "width"
              :type      "number"
              :value     @height
              :on-change #(reset! height (-> % .-target .-value))}]]

    [:div {:class "form-group"}
     [:label {:for "percentageTrigger"} "Percentage trigger for random generation"]
     [:input {:id        "width"
              :type      "number"
              :min       "0"
              :max       "100"
              :value     @percentageTrigger
              :on-change #(reset! percentageTrigger (-> % .-target .-value))}]]

    [:button {:class    "btn btn-primary"
              :on-click #(reset! grid (generate-grid))} "Generate grid"]

    [:button {:class    "btn btn-primary"
              :on-click #(switch-refreshing)} (if @refreshing-grid? "Stop" "Start")]]

   [:svg {:width (* 10 @width) :height (* 10 @height)}
    (draw-grid)]
   ])

;; Render the root component
(defn start []
  (r/render-component
    [game-of-life]
    (.getElementById js/document "root")))
