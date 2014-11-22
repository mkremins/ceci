(ns ceci.compiler
  (:require [clojure.string :as string]
            [ceci.analyzer :as analyzer]
            [ceci.emitter :as emitter]
            [ceci.reader :as reader]
            [ceci.repl :as repl])
  (:refer-clojure :exclude [compile slurp spit]))

(enable-console-print!)

(def fs (js/require "fs"))

(defn slurp [fpath]
  (.readFileSync fs fpath))

(defn spit [fpath contents]
  (.writeFileSync fs fpath contents))

(defn write-js [cljs-source]
  (->> (reader/read-code cljs-source)
       (map analyzer/analyze!)
       (emitter/emit-all)))

(defn compile [in-file out-file]
  (spit out-file (write-js (slurp in-file))))

(defn -main
  ([] (repl/launch!))
  ([in-file out-file]
    (compile in-file out-file)))

(set! *main-cli-fn* -main)
