(defproject game-of-life "0.1.0-SNAPSHOT"
  :description "A game of life experiment with Reagent and Clojurescript"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [reagent "0.2.0"]]
  :plugins [[lein-cljsbuild "1.0.1"]]
  :cljsbuild {:builds [{:source-paths ["src-cljs"]
                        :compiler {:output-to "app.js"
                                   :optimizations :whitespace
                                   :preamble ["reagent/react.js"]
                                   :pretty-print true}}]})
