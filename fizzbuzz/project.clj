(defproject fizzbuzz "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot fizzbuzz.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[midje "1.5.1"]]} :uberjar {:aot :all}})
  
