(defproject conenct4AI "0.1.0-SNAPSHOT"
    :description "an AI that plays connect4"
    :dependencies [[org.clojure/clojure "1.8.0"]
                   [clojure-lanterna "0.9.7"]
                   [org.clojure/core.match "0.3.0-alpha4"]]
    :main connect4AI.core
    :target-path "target/%s"
    :profiles {:uberjar {:aot :all}})
