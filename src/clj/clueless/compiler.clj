(ns clueless.compiler
  (:require [clojure.string :as string]
            [clueless.analyzer :as ana]
            [clueless.emitter :as emt]
            [clueless.reader :as rdr])
  (:refer-clojure :exclude [compile]))

(defn write-js [cljs-source]
  (str (->> cljs-source
         (rdr/read-code)
         (map ana/analyze)
         (map emt/emit)
         (string/join ";\n")) ";"))

(defn compile [in-file out-file]
  (spit out-file (write-js (slurp in-file))))
