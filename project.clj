(defproject ceci "0.0-SNAPSHOT"
  :description "Self-hosting ClojureScript compiler"
  :url "https://github.com/mkremins/ceci"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}

  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2850"]
   [mkremins/fs "0.3.0"]
   [quile/dependency-cljs "0.1.4"]]

  :node-dependencies
  [[escodegen "1.4.1"]
   [mkdirp "0.5.0"]]

  :plugins
  [[lein-cljsbuild "1.0.4"]
   [lein-npm "0.4.0"]]

  :cljsbuild
  {:builds [{:source-paths ["src"]
             :compiler {:main ceci.compiler
                        :output-to "target/ceci.js"
                        :output-dir "target"
                        :optimizations :none
                        :target :nodejs}}]})
