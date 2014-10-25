(ns ceci.analyzer
  (:refer-clojure :exclude [ns-name resolve])
  (:require [ceci.emitter :as emitter]
            [ceci.util :refer [raise update]]
            [clojure.string :as string]))

;; namespace management

(def namespaces (atom {}))
(def ns-name (atom nil))

(defn require-ns
  "Adds a dependency on the `required` namespace to `ns-spec`. If `alias` is
  provided, `required` will be aliased to `alias`; otherwise, it will be
  required under its own fully qualified name."
  ([ns-spec required]
    (require-ns ns-spec required nil))
  ([ns-spec required alias]
    (update ns-spec :required merge {(or alias required) required})))

(defn refer-symbols
  "Within `ns-spec`, refers all symbols in `referred` to the symbols with the
  same names defined in the `required` namespace."
  [ns-spec required referred]
  (update ns-spec :referred merge (into {} (map #(-> [% required]) referred))))

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
  [ns-spec [clause-type & libspecs :as form]]
  (when-not (= clause-type :require)
    (raise "only :require forms permitted in namespace declaration" form))
  (reduce (fn [ns-spec libspec]
            (cond
              (symbol? libspec)
                (require-ns ns-spec libspec)
              (vector? libspec)
                (let [[required & {referred :refer alias :as}] libspec]
                  (-> ns-spec
                      (require-ns required alias)
                      (refer-symbols required referred)))
              :else
                (raise "invalid libspec" libspec)))
          ns-spec libspecs))

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
    (let [ns   (namespace sym)
          name (name sym)
          ns   (or (resolve-ns-alias ns ns-spec)
                   (when (or (namespace-named ns) (= ns "js")) ns)
                   (resolve-defining-ns name ns-spec)
                   (str @ns-name))]
      (symbol ns name))))

;; AST creation

(defn node-type [form]
  (cond (or (true? form) (false? form)) :boolean
        (keyword? form) :keyword
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

;; AST analysis

(declare analyze)

(def analyzed-defs (atom {}))

(defn expr-env [env]
  (assoc env :context :expr))

(defn analyze-block [env exprs]
  (let [body-env (assoc env :context :statement)
        return-env (assoc env :context (if (= (:context env) :statement)
                                           :statement :return))]
    (conj (mapv (partial analyze body-env) (butlast exprs))
          (analyze return-env (last exprs)))))

(defmulti analyze-list (fn [_ {:keys [form]}] (first form)))

;; simple special forms

(defmethod analyze-list 'aget [env {[_ target & fields] :children :as ast}]
  (assoc ast :op :aget
    :target (analyze (expr-env env) target)
    :fields (map (partial analyze (expr-env env)) fields)))

(defmethod analyze-list 'aset [env {[_ target & fields+value] :children :as ast}]
  (let [fields (drop-last fields+value)
        value (last fields+value)]
    (assoc ast :op :aset
      :target (analyze (expr-env env) target)
      :fields (map (partial analyze (expr-env env)) fields)
      :value (analyze (expr-env env) value))))

(defmethod analyze-list 'def [env {[_ name & [init?]] :children :as ast}]
  (let [name-node (analyze (expr-env env) name)
        name-form (:form name-node)
        init-node (analyze (expr-env env) (or init? (form->ast nil)))]
    (swap! analyzed-defs assoc name-form init-node)
    (assoc ast :op :def :name name-node :init init-node)))

(defmethod analyze-list 'deftype* [env {[_ name fields & specs] :children :as ast}]
  (assoc ast :op :deftype
    :name (analyze (expr-env env) name)
    :fields (map :form (:children fields))))

(defmethod analyze-list 'do [env {[_ & body] :children :as ast}]
  (assoc ast :op :do
    :body (analyze-block env body)))

(defmethod analyze-list 'if [env {[_ test then else] :children :as ast}]
  (assoc ast :op :if
    :test (analyze (expr-env env) test)
    :then (analyze env then)
    :else (analyze env else)))

(defmethod analyze-list 'new [env {[_ ctor & args] :children :as ast}]
  (assoc ast :op :new
    :ctor (analyze (expr-env env) ctor)
    :args (map (partial analyze (expr-env env)) args)))

(defmethod analyze-list 'ns [env {[_ ns-name] :children :as ast}]
  (enter-ns! (:form ast))
  (assoc ast :op :ns :name (:form ns-name)))

(defmethod analyze-list 'quote [env {[_ ast] :children}]
  (analyze (assoc env :quoted? true) ast))

(defmethod analyze-list 'throw [env {[_ thrown] :children :as ast}]
  (assoc ast :op :throw
    :thrown (analyze (expr-env env) thrown)))

;; fn forms

(defn analyze-method [env [params & body]]
  (let [params (map :form (:children params))
        _ (assert (every? symbol? params))
        variadic? (some #{'&} params)
        params (vec (remove #{'&} params))
        env (-> env (update :locals concat params) (assoc :context :return))]
    {:op :fn-method
     :params params
     :variadic? variadic?
     :fixed-arity (count (if variadic? (butlast params) params))
     :body (analyze-block env body)}))

(defmethod analyze-list 'fn* [env {[_ & more] :children :as ast}]
  (let [[local more] (if (= (:type (first more)) :symbol)
                       [(first more) (rest more)] [nil more])
        methods (map (partial analyze-method env)
                     (if (= (:type (first more)) :vector)
                       (list more) (map :children more)))
        num-variadic-methods (count (filter :variadic? methods))]
    (when (> num-variadic-methods 1)
      (raise "only one variadic method allowed per function"))
    (assoc ast :op :fn
      :local (:form local (gensym "fn_"))
      :max-fixed-arity (apply max (map :fixed-arity methods))
      :methods methods
      :variadic? (> num-variadic-methods 0))))

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
    (if-let [[bform bound] (get bindings idx)]
      (do (assert (symbol? bform))
          (recur (update env :locals conj bform)
                 (conj analyzed [bform (analyze (expr-env env) bound)])
                 (inc idx)))
      [env analyzed])))

(defmethod analyze-list 'let* [env {[_ bindings & body] :children :as ast}]
  (let [[body-env bindings] (analyze-bindings env (compile-bindings bindings))]
    (assoc ast :op :let
      :bindings bindings
      :body (analyze-block body-env body))))

;; loop and recur forms

(defmethod analyze-list 'loop* [env {[_ bindings & body] :children :as ast}]
  (let [[body-env bindings] (analyze-bindings env (compile-bindings bindings))
        ast (assoc ast :op :loop :bindings bindings)
        body-env (assoc body-env :recur-point ast)]
    (assoc ast :body (analyze-block body-env body))))

(defmethod analyze-list 'recur [env {[_ & args] :children :as ast}]
  (let [recur-point (:recur-point env)]
    (if recur-point
        (assoc ast :op :recur
          :recur-point recur-point
          :args (vec (map (partial analyze (expr-env env)) args)))
        (raise "can't recur here – no enclosing loop" (:form ast)))))

;; generic interface

(defn analyze-coll [env ast]
  (update ast :children #(map (partial analyze (expr-env env)) %)))

(defmethod analyze-list :default [env {:keys [form children] :as ast}]
  (if (or (:quoted? env) (empty? children))
    (analyze-coll env ast)
    (assoc ast :op :invoke
      :invoked (analyze (expr-env env) (first children))
      :args (map (partial analyze (expr-env env)) (rest children)))))

(defn analyze-symbol [env {sym :form :as ast}]
  (cond (:quoted? env) ast
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
