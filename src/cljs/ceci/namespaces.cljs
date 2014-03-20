(ns ceci.namespaces
  (:refer-clojure :exclude [ns-name resolve])
  (:require [clojure.string :as string]
            [ceci.util :refer [raise update]]))

;; namespace management

(def namespaces (atom {}))
(def ns-name (atom nil))

(defn require-ns
  "Adds a dependency on the namespace `required-ns` to `ns-spec`. If `ns-alias`
  is provided, `required-ns` will be aliased to `ns-alias`; otherwise, it will
  be required under its own fully qualified name."
  ([ns-spec required-ns]
    (require-ns ns-spec required-ns required-ns))
  ([ns-spec required-ns ns-alias]
    (update ns-spec :required merge {ns-alias required-ns})))

(defn refer-symbols
  "Within `ns-spec`, refers all symbols in `referred-symbols` to the symbols
  with the same names defined in `required-ns`."
  [ns-spec required-ns referred-symbols]
  (if referred-symbols
      (update ns-spec :referred merge
              (->> referred-symbols
                   (map (juxt identity (constantly required-ns)))
                   (into {})))
      ns-spec))

(def core-defs
  '[+ - * / = > >= < <= and apply assoc assoc-in boolean comp concat conj cons
    constantly dec dissoc filter fnil get get-in hash hash-map identity
    inc interpose into juxt key keys keyword keyword? list list? map map? merge
    nil? not not= number? or partial print println pr prn pr-str reduce remove
    reset! seq seq? set set? str swap! update-in val vals vec vector vector?])

(defn add-clause
  "Given a namespace specification `ns-spec` and a :require form from a
  namespace declaration, parses the :require form and returns a modified copy
  of `ns-spec` with the appropriate dependency information added."
  [ns-spec [clause-type & libspecs]]
  (if (not= clause-type :require)
      ns-spec ; ignore clauses that aren't of form (:require ...)
      (reduce (fn [ns-spec libspec]
                (cond (symbol? libspec) (require-ns ns-spec libspec)
                      (vector? libspec)
                      (let [required-ns (first libspec)
                            {referred-syms :refer ns-alias :as
                             :or {referred-syms [] ns-alias required-ns}}
                            (apply hash-map (rest libspec))]
                        (-> ns-spec
                            (require-ns required-ns ns-alias)
                            (refer-symbols required-ns referred-syms)))
                      :else (raise "invalid libspec" libspec)))
              ns-spec libspecs)))

(defn parse-ns-decl
  "Given a namespace declaration form, parses it and returns a valid namespace
  specification – a map containing keys :name, :required and :referred, where:

     :name => a symbol that gives the namespace's fully qualified name
     :required => a map from local namespace aliases to required namespaces'
       fully qualified names
     :referred => a map from locally referred symbols to their defining
       namespaces' fully qualified names

  By default, all namespaces depend on `cljs.core` and refer all the symbols in
  `core-defs` from the core namespace."
  [[_ name & clauses]]
  (let [ns-spec (refer-symbols {:name name} 'cljs.core core-defs)]
    (reduce add-clause ns-spec clauses)))

(defn enter-ns!
  "Given a namespace declaration form `ns-decl`, parses it to produce a
  namespace specification (using `parse-ns-decl`) and immediately switches into
  the newly created namespace."
  [ns-decl]
  (let [{:keys [name] :as ns-spec} (parse-ns-decl ns-decl)]
    (swap! namespaces assoc name ns-spec)
    (reset! ns-name name)
    ns-spec))

(enter-ns! '(ns user))

;; symbol expansion

(defn symbol-parts [sym]
  (let [sym-str (str sym)
        parts (string/split sym-str #"/" 2)]
    (cond (every? empty? parts) [nil "/"]
          (= (count parts) 1) [nil (first parts)]
          :else parts)))

(defn resolve-ns-alias [ns-alias ns-spec]
  (when ns-alias (get-in ns-spec [:required (symbol ns-alias)])))

(defn resolve-defining-ns [sym-name ns-spec]
  (get-in ns-spec [:referred (symbol sym-name)]))

(defn namespace-named [ns-name]
  (get @namespaces (symbol ns-name)))

(defn resolve
  "Given a potentially unqualified or only partly qualified symbol `sym`,
  returns the fully qualified version of that symbol in the context of
  namespace specification `ns-spec` (defaulting to the current working
  namespace specification if none is specified)."
  ([sym] (resolve sym (namespace-named @ns-name)))
  ([sym ns-spec]
    (let [[ns-part name-part] (symbol-parts sym)
          ns-part (or (resolve-ns-alias ns-part ns-spec)
                      (when (or (namespace-named ns-part) (= ns-part "js"))
                            ns-part)
                      (resolve-defining-ns name-part ns-spec)
                      (str @ns-name))]
      (symbol ns-part name-part))))
