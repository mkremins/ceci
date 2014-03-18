(ns ceci.compiler
  (:require [clojure.string :as string]
            [ceci.analyzer :as analyzer]
            [ceci.generator :as emitter]
            [ceci.expander :as expander]
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
  (->> cljs-source
       (reader/read-code)
       (map expander/expand-all)
       (map analyzer/form->ast)
       (map analyzer/analyze)
       (map emitter/emit)
       (map #(if (= (last %) ";")
                 (str % "\n\n")
                 (str % ";\n\n")))
       (string/join)))

(defn compile [in-file out-file]
  (spit out-file (write-js (slurp in-file))))

(defn -main
  ([] (repl/launch!))
  ([in-file out-file]
    (compile in-file out-file)))

(set! *main-cli-fn* compile) ;; TODO this is only temporary!
