(ns ceci.analyzer
  (:refer-clojure :exclude [ns-name resolve])
  (:require [ceci.emitter :as emitter]
            [ceci.util :refer [in? raise update]]
            [clojure.string :as string]))

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
  '[+ - * / = > >= < <= and apply assoc assoc-in atom boolean comp concat conj
    cons constantly dec dissoc empty? filter first fnil gensym get get-in hash
    hash-map identity inc interleave interpose into juxt key keys keyword
    keyword? list list? map map? merge nil? not not= number? or partial
    partition print println pr prn pr-str reduce remove reset! rest reverse
    second seq seq? set set? str swap! update-in val vals vec vector vector?])

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

;; AST creation

(defn node-type [form]
  (cond (keyword? form) :keyword
        (or (list? form) (seq? form)) :list
        (map? form) :map
        (nil? form) :nil
        (number? form) :number
        (set? form) :set
        (string? form) :string
        (symbol? form) :symbol
        (vector? form) :vector
        :else (raise "unrecognized form type" form)))

(defn form->ast [form]
  (let [type (node-type form)
        ast {:form form :meta (meta form) :type type}]
    (if (coll? form)
        (assoc ast :op :coll :children (map form->ast form))
        (assoc ast :op :const))))

(def true-ast-node
  {:op :const :type :bool :form true})

(def false-ast-node
  {:op :const :type :bool :form false})

(def nil-ast-node
  {:op :const :type :nil :form nil})

;; AST analysis

(declare analyze)

(def analyzed-defs (atom {}))

(defn expr-env [env]
  (assoc env :context :expr))

(defn analyze-block [env exprs]
  (let [body-env (assoc env :context :statement)
        return-env (assoc env :context (if (= (:context env) :statement)
                                           :statement :return))]
    (conj (vec (map (partial analyze body-env) (butlast exprs)))
          (analyze return-env (last exprs)))))

;; simple special forms

(defn analyze-aget [env {[_ target & fields] :children :as ast}]
  (assoc ast :op :aget
    :target (analyze (expr-env env) target)
    :fields (map (partial analyze (expr-env env)) fields)))

(defn analyze-aset [env {[_ target & fields+value] :children :as ast}]
  (let [fields (drop-last fields+value)
        value (last fields+value)]
    (assoc ast :op :aset
      :target (analyze (expr-env env) target)
      :fields (map (partial analyze (expr-env env)) fields)
      :value (analyze (expr-env env) value))))

(defn analyze-def [env {[_ name & [init?]] :children :as ast}]
  (let [name-node (analyze (expr-env env) name)
        name-form (:form name-node)
        init-node (analyze (expr-env env) (or init? nil-ast-node))]
    (when (contains? @analyzed-defs name-form)
      (raise "illegal redefinition of an already-defined symbol" (:form ast)))
    (swap! analyzed-defs assoc name-form init-node)
    (assoc ast :op :def :name name-node :init init-node)))

(defn analyze-deftype [env {[_ name fields & specs] :children :as ast}]
  (assoc ast :op :deftype
    :name (analyze (expr-env env) name)
    :fields (map :form (:children fields))))

(defn analyze-do [env {[_ & body] :children :as ast}]
  (assoc ast :op :do
    :body (analyze-block env body)))

(defn analyze-if [env {[_ test then else] :children :as ast}]
  (assoc ast :op :if
    :test (analyze (expr-env env) test)
    :then (analyze env then)
    :else (analyze env else)))

(defn analyze-new [env {[_ ctor & args] :children :as ast}]
  (assoc ast :op :new
    :ctor (analyze (expr-env env) ctor)
    :args (map (partial analyze (expr-env env)) args)))

(defn analyze-ns [env {[_ ns-name] :children :as ast}]
  (enter-ns! (:form ast))
  (assoc ast :op :ns :name (:form ns-name)))

(defn analyze-quote [env {[_ ast] :children}]
  (analyze (assoc env :quoted? true) ast))

(defn analyze-throw [env {[_ thrown] :children :as ast}]
  (assoc ast :op :throw
    :thrown (analyze (expr-env env) thrown)))

;; fn forms

(defn analyze-clause-body [env exprs]
  (let [body-env (assoc env :context :statement)
        return-env (assoc env :context :return)]
    (conj (vec (map (partial analyze body-env) (butlast exprs)))
          (analyze return-env (last exprs)))))

(defn analyze-clauses [env clauses]
  (loop [analyzed {} allow-variadic? true clauses clauses]
    (if-let [[params & body] (first clauses)]
      (let [params (map :form (:children params))
            body-env (update env :locals concat params)
            variadic? (in? params '&)
            clause {:params (vec (remove #{'&} params)) :variadic? variadic?
                    :body (analyze-clause-body body-env body)}]
        (when (and variadic? (not allow-variadic?))
          (raise "only one variadic clause allowed per function"))
        (recur (assoc analyzed (count params) clause)
               (not variadic?) (rest clauses)))
      analyzed)))

(defn extract-clauses
  "Given `ast`, an AST node representing a `fn` special form, extracts and
  returns a collection of clauses. Each clause is a sequence of AST nodes whose
  first item represents the params taken by that clause and whose remaining
  items comprise the clause body."
  [{[_ & args] :children :as ast}]
  (condp = (:type (first args))
    :symbol (let [args (rest args)]
              (condp = (:type (first args))
                :vector [(cons (first args) (rest args))]
                :list (map :children args)
                (raise "invalid function definition" (:form ast))))
    :vector [(cons (first args) (rest args))]
    :list (map :children args)
    (raise "invalid function definition" (:form ast))))

(defn analyze-fn [env ast]
  (assoc ast :op :fn
    :clauses (analyze-clauses env (extract-clauses ast))))

;; let forms

(defn compile-bindings [bindings]
  (if (= (:type bindings) :vector)
    (let [pairs (partition 2 (:children bindings))]
      (if (= (count (last pairs)) 2)
        (vec (map (juxt (comp :form first) second) pairs))
        (raise "number of forms in bindings vector must be even" bindings)))
    (raise "bindings form must be vector" bindings)))

(defn analyze-bindings [env bindings]
  (loop [env env analyzed [] idx 0]
    (if-let [[left-hand right-hand] (get bindings idx)]
      (recur (update env :locals conj left-hand)
             (conj analyzed [left-hand (analyze (expr-env env) right-hand)])
             (inc idx))
      [env analyzed])))

(defn analyze-let [env {[_ bindings & body] :children :as ast}]
  (let [[body-env bindings] (analyze-bindings env (compile-bindings bindings))]
    (assoc ast :op :let
      :bindings bindings
      :body (analyze-block body-env body))))

;; loop and recur forms

(defn analyze-loop [env {[_ bindings & body] :children :as ast}]
  (let [[body-env bindings] (analyze-bindings env (compile-bindings bindings))
        ast (assoc ast :op :loop :bindings bindings)
        body-env (assoc body-env :recur-point ast)]
    (assoc ast :body (analyze-block body-env body))))

(defn analyze-recur [env {[_ & args] :children :as ast}]
  (let [recur-point (:recur-point env)]
    (if recur-point
        (assoc ast :op :recur
          :recur-point recur-point
          :args (vec (map (partial analyze (expr-env env)) args)))
        (raise "can't recur here – no enclosing loop" (:form ast)))))

;; generic interface

(defn analyze-coll [env ast]
  (update ast :children #(map (partial analyze (expr-env env)) %)))

(def specials
  {'aget analyze-aget
   'aset analyze-aset
   'def analyze-def
   'deftype* analyze-deftype
   'do analyze-do
   'fn* analyze-fn
   'if analyze-if
   'let* analyze-let
   'loop* analyze-loop
   'new analyze-new
   'ns analyze-ns
   'quote analyze-quote
   'recur analyze-recur
   'throw analyze-throw})

(defn analyze-list [env {:keys [form children] :as ast}]
  (if (or (:quoted? env) (empty? children))
      (analyze-coll env ast)
      (if-let [analyze-special (specials (first form))]
        (analyze-special env ast)
        (assoc ast :op :invoke
          :invoked (analyze (expr-env env) (first children))
          :args (map (partial analyze (expr-env env)) (rest children))))))

(defn analyze-symbol [env {sym :form :as ast}]
  (cond (:quoted? env) ast
        (= sym (symbol "true")) true-ast-node
        (= sym (symbol "false")) false-ast-node
        (= sym (symbol "nil")) nil-ast-node
        ((set (:locals env)) sym) ast
        :else (assoc ast :form (resolve sym))))

(defn analyze
  ([ast] (analyze {:context :statement :locals [] :quoted? false} ast))
  ([env {:keys [op type] :as ast}]
    (let [ast (assoc ast :env env)]
      (cond (= type :list) (analyze-list env ast)
            (= op :coll) (analyze-coll env ast)
            (= type :symbol) (analyze-symbol env ast)
            :else ast))))
