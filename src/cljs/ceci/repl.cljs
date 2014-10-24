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
       (emitter/emit-all)))

(defn eval-print!
  "Compiles a `line` of ClojureScript code to JavaScript, then immediately
  evaluates the resulting JavaScript in the context of REPL interface
  `interface` and prints the result."
  [interface line]
  (let [res (try (js/eval (write-js line))
              (catch js/Error e e))]
    (prn res)
    (.prompt interface)))

(defn log!
  "Logs every message in `messages` to stdout in order, separating them with
  newlines."
  [& messages]
  (let [messages-str (string/join "\n" messages)]
    (println messages-str)))

(defn init!
  "Handles initial setup of the REPL, initializing an empty `user` namespace in
  which the REPL user may work and printing the REPL startup banner to stdout."
  []
  (js/eval "user = {};")
  (log! "Ceci ClojureScript REPL v0.1.0"
        "Clojure 1.6.0 / ClojureScript 0.0-2371"
        "(CTRL+C to quit)\n"))

(defn launch!
  "Launches the REPL, first initializing a new REPL instance (using `init!`)
  and then handing control of stdin and stdout to the created instance."
  []
  (init!)
  (let [opts #js {:input (.-stdin js/process) :output (.-stdout js/process)}
        interface (.createInterface rl opts)]
    (doto interface
      (.setPrompt "user=> ")
      (.prompt)
      (.on "line" (partial eval-print! interface)))))
