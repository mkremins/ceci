(ns ceci.repl
  (:require [clojure.string :as str]))

(def readline (js/require "readline"))

(def banner
  (str/join "\n"
    ["Ceci ClojureScript REPL v0.0-SNAPSHOT"
     "Clojure 1.6.0 / ClojureScript 0.0-2371"
     "(CTRL+C to quit)\n"]))

(defn launch!
  "Launches the REPL, handing control of stdin and stdout to the REPL session."
  [write-js]
  (println banner)
  (js/eval "user = {};") ;; initialize the user namespace
  (let [cli (.createInterface readline
              #js {:input (.-stdin js/process) :output (.-stdout js/process)})]
    (doto cli
      (.setPrompt "user=> ")
      (.prompt)
      (.on "line" #(do (prn (try (js/eval (write-js %)) (catch js/Error e e)))
                       (.prompt cli))))))
