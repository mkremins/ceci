(ns clueless.compiler
  (:require [clojure.string :as string]
            [clueless.analyzer :as ana]
            [clueless.emitter :as emt]
            [clueless.reader :as rdr])
  (:refer-clojure :exclude [compile]))

(defn compile [in-file out-file]
  (->> (slurp in-file)
    (rdr/read-code)
    (map ana/expand)
    (map emt/emit)
    (string/join ";\n")
    (spit out-file)))
