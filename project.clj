(defproject ceci "0.1.0-SNAPSHOT"
  :description "Self-hosting ClojureScript compiler"
  :url "http://github.com/mkremins/ceci"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}

  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2371"]]

  :node-dependencies
  [[escodegen "1.4.1"]]

  :plugins
  [[lein-cljsbuild "1.0.3"]
   [lein-npm "0.4.0"]]

  :cljsbuild
  {:builds [{:source-paths ["src"]
             :compiler {:output-to "target/ceci.js"
                        :output-dir "target/out"
                        :optimizations :simple
                        :target :nodejs}}]})
