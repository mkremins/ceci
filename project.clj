(defproject ceci "0.1.0-SNAPSHOT"
  :description "Self-hosting ClojureScript compiler"
  :url "http://github.com/mkremins/ceci"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}

  :dependencies
  [[org.clojure/clojure "1.5.1"]
   [org.clojure/clojurescript "0.0-2173"]]

  :node-dependencies
  [[escodegen "1.3.0"]]

  :plugins
  [[lein-cljsbuild "1.0.2"]
   [lein-npm "0.3.0"]]

  :cljsbuild
  {:builds [{:source-paths ["src/cljs"]
             :compiler {:output-to "target/js/ceci.js"
                        :output-dir "target/js/out"
                        :optimizations :simple
                        :target :nodejs}}]})
