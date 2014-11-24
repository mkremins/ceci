(ns ceci.analyzer
  (:refer-clojure :exclude [ns-name resolve])
  (:require [ceci.emitter :as emitter]
            [ceci.util :refer [merge-meta raise update]]))

(def state (atom {:current-ns nil :macros {} :namespaces {}}))

;; namespace management

(defn require-ns
  ([ns-spec required]
    (require-ns ns-spec required nil))
  ([ns-spec required alias]
    (let [required (str required)
          alias (if alias (str alias) required)]
      (update ns-spec :aliases merge {alias required}))))

(defn refer-symbols [ns-spec from-ns syms]
  (update ns-spec :referred merge (zipmap syms (repeat (str from-ns)))))

(def core-defs
  '[+ - * / = > >= < <= aget and apply aset assoc assoc-in atom boolean comp
    concat conj cons constantly dec defmacro dissoc empty? filter first fnil
    gensym get get-in hash hash-map identity inc interleave interpose into juxt
    key keys keyword keyword? list list? map map? merge nil? not not= number?
    or partial partition print println pr prn pr-str reduce remove reset! rest
    reverse second seq seq? set set? str syntax-quote swap! update-in val vals
    vec vector vector?])

(defn create-ns-spec [ns-name]
  (-> {:name ns-name}
      (require-ns "cljs.core") (refer-symbols "cljs.core" core-defs)))

(defn enter-ns [state {:keys [name] :as ns-spec}]
  (-> state (assoc-in [:namespaces name] ns-spec) (assoc :current-ns name)))

(swap! state enter-ns (create-ns-spec "user"))

;; symbol expansion

(defn resolve-ns [ns-name]
  (let [{:keys [current-ns namespaces]} @state]
    (or (namespaces ns-name)
        (when-let [ns-name* (get-in namespaces [current-ns :aliases ns-name])]
          (namespaces ns-name*)))))

(defn resolve
  "Returns the canonical expansion of `sym` in the context of `ns-spec`. Uses
  the currently active namespace if no `ns-spec` is provided."
  ([sym] (resolve sym (resolve-ns (:current-ns @state))))
  ([sym ns-spec]
    (if-let [sym-ns (namespace sym)]
      (if (= sym-ns "js")
        sym
        (symbol (:name (resolve-ns sym-ns)) (name sym)))
      (symbol (get (:referred ns-spec) sym (:name ns-spec)) (str sym)))))

;; macroexpansion & syntax desugaring

(defn expand-macro [form]
  (if-let [macro (get-in @state [:macros (resolve (first form))])]
    (let [metadata (meta form)]
      (merge-meta (apply macro (rest form)) metadata))
    form))

(defn desugar-new-syntax
  "Desugars (Ctor. args) to (new Ctor args)."
  [[ctor & args :as form]]
  (let [cname (name ctor)]
    (if (= (last cname) \.)
      (let [cname (apply str (drop-last cname))
            cns (namespace ctor)]
        (list* 'new (if cns (symbol cns cname) (symbol cname)) args))
      form)))

(defn expand-once [form]
  (if (and (list? form) (symbol? (first form)))
    (-> form expand-macro desugar-new-syntax)
    form))

(defn expand
  "Expands `form` repeatedly using `expand-once` until the result cannot be
  expanded further, then returns the result."
  [form]
  (loop [original form
         expanded (expand-once form)]
    (if (= original expanded)
      original
      (recur expanded (expand-once expanded)))))

(defn expand-all
  [form]
  "Walks `form` from the top down, applying `expand` to each subform, and
  returns the result."
  (let [form (expand form)
        metadata (meta form)
        expanded (condp #(%1 %2) form
                   map? (apply hash-map (map expand-all (flatten1 form)))
                   (some-fn list? seq?) (map expand-all form)
                   coll? (into (empty form) (map expand-all form))
                   form)]
    (merge-meta expanded metadata)))

;; syntax-quote

(defn syntax-quote [form]
  (let [unquote? (every-pred list? (comp #{'unquote} first))
        unquote-splicing? (every-pred list? (comp #{'unquote-splicing} first))
        splice (fn [forms]
                 (cons 'concat
                   (map #(if (unquote-splicing? %)
                           (list 'seq (second %)) [(syntax-quote %)])
                        forms)))]
    (condp #(%1 %2) form
      symbol? (list 'quote form)
      unquote? (second form)
      unquote-splicing? (raise "Invalid location for unquote-splicing." form)
      (some-fn (complement coll?) empty?) form
      list? (splice form)
      map? (list 'apply 'hash-map (splice (flatten1 form)))
      set? (list 'set (splice form))
      vector? (list 'vec (splice form))
      (raise "Unsupported collection type." form))))

(swap! state assoc-in [:macros 'cljs.core/syntax-quote] syntax-quote)

;; AST creation

(defn node-type [form]
  (condp #(%1 %2) form
    (some-fn true? false?) :boolean
    keyword? :keyword
    (some-fn list? seq?) :list
    map? :map
    nil? :nil
    number? :number
    set? :set
    string? :string
    symbol? :symbol
    vector? :vector))

(defn form->ast [form]
  (let [type (node-type form)
        ast {:form form :meta (meta form) :type type}]
    (if (coll? form)
      (assoc ast :op :coll :children (map form->ast form))
      (assoc ast :op :const))))

;; AST analysis

(declare analyze)

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

(defmethod analyze-list 'def [env {[_ name & [init]] :children :as ast}]
  (let [analyze* (partial analyze (expr-env env))]
    (assoc ast :op :def
      :name (analyze* name)
      :init (analyze* (or init (form->ast nil))))))

(defmethod analyze-list 'defmacro [env {[_ sym & args] :form :as ast}]
  (let [analyze* (partial analyze (expr-env env))
        fn-ast (-> (cons 'fn* args) form->ast analyze*)
        macro (js/eval (str "(" (emitter/emit fn-ast) ")"))
        full-sym (symbol (:current-ns @state) (name sym))]
    (swap! state assoc-in [:macros full-sym] macro)
    (assoc ast :op :def
      :name (analyze* (second (:children ast)))
      :init fn-ast)))

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

(defn analyze-bindings [env {:keys [children] :as bindings}]
  (assert (= (:type bindings) :vector))
  (assert (even? (count children)))
  (loop [body-env env
         analyzed []
         pairs (for [[k v] (partition 2 children)] [(:form k) v])]
    (if-let [[bsym bval] (first pairs)]
      (do (assert (symbol? bsym))
          (recur (update body-env :locals conj bsym)
                 (conj analyzed [bsym (analyze (expr-env body-env) bval)])
                 (rest pairs)))
      [body-env analyzed])))

(defmethod analyze-list 'let* [env {[_ bindings & body] :children :as ast}]
  (let [[body-env bindings] (analyze-bindings env bindings)]
    (assoc ast :op :let
      :bindings bindings
      :body (analyze-block body-env body))))

;; loop and recur forms

(defmethod analyze-list 'loop* [env {[_ bindings & body] :children :as ast}]
  (let [[body-env bindings] (analyze-bindings env bindings)
        ast (assoc ast :op :loop :bindings bindings)
        body-env (assoc body-env :recur-point ast)]
    (assoc ast :body (analyze-block body-env body))))

(defmethod analyze-list 'recur [env {[_ & args] :children :as ast}]
  (if-let [recur-point (:recur-point env)]
    (assoc ast :op :recur
      :recur-point recur-point
      :args (mapv (partial analyze (expr-env env)) args))
    (raise "can't recur here – no enclosing loop" (:form ast))))

;; ns forms

(defn add-libspec [ns-spec libspec]
  (condp #(%1 %2) libspec
    symbol? (require-ns ns-spec libspec)
    vector? (let [[required & {alias :as, referred :refer}] libspec]
              (-> ns-spec (require-ns required alias)
                          (refer-symbols required referred)))
    (raise "Only vectors and symbols may be used as libspecs." libspec)))

(defn add-require-clause [ns-spec [head & libspecs :as form]]
  (when-not (= head :require)
    (raise "The ns macro supports only :require at this time." form))
  (reduce add-libspec ns-spec libspecs))

(defmethod analyze-list 'ns [env {[_ ns-sym & clauses] :form :as ast}]
  (let [ns-spec (create-ns-spec (str ns-sym))]
    (swap! state enter-ns (reduce add-require-clause ns-spec clauses)))
  (assoc ast :op :ns :name ns-sym))

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

(defn analyze-form [form]
  (-> form expand-all form->ast analyze))
