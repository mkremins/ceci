(ns clueless.compiler
  (:require [clojure.string :as string]
            [clueless.analyzer :as ana]
            [clueless.emitter :as emt]
            [clueless.reader :as rdr])
  (:refer-clojure :exclude [compile]))

(defn compile [fpath]
  (let [source (slurp fpath)
        ast (rdr/read-code source)
        ast2 (map ana/expand ast)
        out (map emt/emit ast2)]
    (println source)
    (println ast)
    (println ast2)
    (println out)))
