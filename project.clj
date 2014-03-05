(defproject clueless "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]]
  :plugins [[lein-cljsbuild "1.0.2"]]

  :cljsbuild {
    :builds [{:source-paths ["src/cljs"]
              :compiler {:output-to "target/js/clueless.js"
                         :output-dir "target/js/out"
                         :optimizations :simple
                         :target :nodejs}}]})
