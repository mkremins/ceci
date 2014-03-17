(ns ceci.repl
  (:require [clojure.string :as string]
            [ceci.analyzer :as analyzer]
            [ceci.emitter :as emitter]
            [ceci.expander :as expander]
            [ceci.reader :as reader]))

(def rl (js/require "readline"))

(defn write-js [cljs-source]
  (->> cljs-source
       (reader/read-code)
       (map expander/expand-all)
       (map analyzer/form->ast)
       (map analyzer/analyze)
       (map emitter/emit)
       (string/join "\n")))

(defn eval-line [interface line]
  (let [res (try (js/eval (write-js line))
              (catch js/Error e e))]
    (.log js/console res)
    (.prompt interface)))

(defn init! []
  (js/eval "user = {};"))

(defn launch! []
  (init!)
  (let [opts #js {:input (.-stdin js/process) :output (.-stdout js/process)}
        interface (.createInterface rl opts)]
    (doto interface
      (.setPrompt "user=> ")
      (.prompt)
      (.on "line" (partial eval-line interface)))))
