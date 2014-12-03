(ns ceci.compiler
  (:require [clojure.string :as str]
            [ceci.analyzer :as analyzer :include-macros true]
            [ceci.emitter :as emitter]
            [ceci.reader :as reader]
            [ceci.repl :as repl]
            [fs.core :as fs]
            [quile.dependency :as dep]))

(enable-console-print!)

(defn fpath->ns-name [fpath]
  (let [fpath (str/replace fpath \_ \-)]
    (as-> (str/split (fs/dirname fpath) #"/") parts
          (conj parts (fs/basename fpath "cljs"))
          (str/join \. parts)
          (symbol parts))))

(defn load-sources
  "Given a `dirpath` from which to load source files, returns a map from
  namespace names to seqs of top-level forms."
  [dirpath]
  (->> (fs/files-seq dirpath)
       (filter #(= (fs/extname %) "cljs"))
       (map #(-> [(fpath->ns-name (fs/rel-path dirpath %))
                  (reader/read-code (fs/slurp %))]))
       (into {'cljs.core []})))

(defn namespace-dep-graph
  "Given a `sources` map like that returned by `load-sources`, returns a graph
  of the dependencies between namespaces."
  [sources]
  (reduce (fn [graph {:keys [aliases name]}]
            (reduce #(dep/depend %1 name %2) graph (vals aliases)))
          (dep/graph) (map (comp analyzer/parse-ns-decl first val) sources)))

(defn precompiled-js [ns-name]
  (when (= ns-name 'cljs.core)
    (fs/slurp "./resources/runtime.js")))

(defn compile-forms
  ([forms] (compile-forms (analyzer/default-state) forms))
  ([state forms]
    (analyzer/with-state state
      (emitter/emit-all (map analyzer/analyze forms)))))

(defn compile-directory [dirpath]
  (let [sources (load-sources dirpath)
        ns-deps (namespace-dep-graph sources)
        state (analyzer/default-state)]
    (->> (sort-by key (dep/topo-comparator ns-deps) sources)
         (map (fn [[ns-name forms]]
                (or (precompiled-js ns-name)
                    (analyzer/with-state state (compile-forms forms)))))
         str/join)))

(defn -main
  ([]
    (let [state (analyzer/default-state)]
      (repl/launch! (comp #(compile-forms state %) reader/read-code))))
  ([source-dir out-file]
    (fs/spit out-file (compile-directory source-dir))))

(set! *main-cli-fn* -main)
