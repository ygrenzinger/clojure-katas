(ns game-of-life.core
  (:require [clojure.string :as string]
            [reagent.core :as r]))

(enable-console-print!)

(def width (r/atom 10))
(def height (r/atom 10))

(defn generate-grid []
  (vec (take @height (repeat (vec (take @width (repeat 0)))))))

(def grid (r/atom (generate-grid)))

;return 0 if out of grid (there is a dead land out there
(defn get-cell-state [row-idx col-idx]
  (get-in @grid [row-idx col-idx] 0))

(defn get-inverted-cell-state [row-idx col-idx]
  (if (= 0 (get-cell-state row-idx col-idx)) 1 0))

(defn switch-cell-state! [row-idx col-idx]
  (swap! grid assoc-in [row-idx col-idx] (get-inverted-cell-state row-idx col-idx)))

(defn neighbours [row-idx col-idx]
  [[(dec row-idx) (dec col-idx)] [(dec row-idx) col-idx] [(dec row-idx) (inc col-idx)]
   [row-idx (dec col-idx)] [row-idx (inc col-idx)]
   [(inc row-idx) (dec col-idx)] [(inc row-idx) col-idx] [(inc row-idx) (inc col-idx)]])

(defn count-alive-neighbours [row-idx col-idx]
  (reduce + (map #(apply get-cell-state %) (neighbours row-idx col-idx))))

(defn alive-cell? [row-idx col-idx]
  (= 1 (get-cell-state row-idx col-idx)))

(defn next-cell-state [row-idx col-idx]
  (let [alive (alive-cell? row-idx col-idx)
        alive-neighbours (count-alive-neighbours row-idx col-idx)]
    (if alive
      (if (and (> alive-neighbours 1) (< alive-neighbours 4))
        1
        0)
      (if (= alive-neighbours 3)
        1
        0))))

(defn next-row [row-idx row]
  (vec (map-indexed (fn [col-idx _] (next-cell-state row-idx col-idx)) row)))

(defn next-grid []
  (vec (map-indexed (fn [row-idx row] (next-row row-idx row)) @grid)))

(defn draw-cell [row-idx col-idx cell-state]
  (let [color (if (= 1 cell-state) "black" "white")]
    ^{:key (str row-idx col-idx)} [:div
                                   {:on-click #(switch-cell-state! row-idx col-idx)
                                    :style
                                              {:border           "1px solid"
                                               :display          "inline-block"
                                               :background-color color
                                               :width            "20px"
                                               :height           "20px"}
                                    }
                                   ""]))

(defn draw-row [row-idx row]
  ^{:key (str row-idx)} [:div
                         {:style {:height "20px"}}
                         (map-indexed (fn [idx cell] (draw-cell row-idx idx cell)) row)])

(defn draw-grid []
  (map-indexed draw-row @grid))

(defn refresh-grid []
  (let [next-grid (next-grid)]
    #(reset! grid next-grid)))


;(js/setTimeout  1000)


(defn game-of-life []
  [:div {:class "container"}
   [:h1 "Game of life"]
   [:form {:class "form-inline"}

    [:div {:class "form-group"}
     [:label {:for "width"} "width"]
     [:input {:id        "width"
              :type      "number"
              :value     @width
              :on-change #(reset! width (-> % .-target .-value))}]]

    [:div {:class "form-group"}
     [:label {:for "height" :class "mdl-textfield__label"} "height"]
     [:input {:id        "width"
              :type      "number"
              :value     @height
              :on-change #(reset! height (-> % .-target .-value))}]]

    [:button {:class    "btn btn-primary"
              :on-click #(reset! grid (generate-grid))} "Generate grid"]

    [:button {:class    "btn btn-primary"
              :on-click #(println "test ")} "Start"]]

   [:div {:class "container grid"}
    (draw-grid)]
   ])

;; Render the root component
(defn start []
  (r/render-component
    [game-of-life]
    (.getElementById js/document "root")))
