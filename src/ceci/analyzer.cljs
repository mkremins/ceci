(ns ceci.analyzer
  (:refer-clojure :exclude [macroexpand macroexpand-1 resolve])
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
    (apply macro (rest form))
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

(defn macroexpand-1 [form]
  (if (and (list? form) (symbol? (first form)))
    (-> form expand-macro desugar-new-syntax)
    form))

(defn macroexpand
  "Macroexpands `form` repeatedly until the result cannot be expanded further,
  then returns the result."
  [form]
  (loop [original form
         expanded (macroexpand-1 form)]
    (if (= expanded original)
      (merge-meta expanded (meta form))
      (recur expanded (macroexpand-1 expanded)))))

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
    seq? :list
    map? :map
    nil? :nil
    number? :number
    set? :set
    string? :string
    symbol? :symbol
    vector? :vector))

(defn ast [op form env & attrs]
  (merge {:op op :form form :env env :meta (meta form) :type (node-type form)}
         (apply hash-map attrs)))

(declare analyze)

(defn expr-env [env]
  (assoc env :context :expr))

(defn analyze-block [env forms]
  (let [body-env (assoc env :context :statement)
        return-env (update env :context #(if (= % :statement) % :return))]
    (conj (mapv (partial analyze body-env) (butlast forms))
          (analyze return-env (last forms)))))

(defmulti analyze-list (fn [_ form] (first form)))

;; special forms

(defmethod analyze-list 'def [env [_ name init :as form]]
  (ast :def form env
    :name (analyze (expr-env env) name)
    :init (analyze (expr-env env) init)))

(defmethod analyze-list 'defmacro [env [_ sym & more :as form]]
  (let [fn-ast (analyze (expr-env env) (cons 'fn* more))
        macro (js/eval (str "(" (emitter/emit fn-ast) ")"))
        full-sym (symbol (:current-ns @state) (name sym))]
    (swap! state assoc-in [:macros full-sym] macro)
    (ast :def form env
      :name (analyze (expr-env env) sym)
      :init fn-ast)))

(defmethod analyze-list 'deftype* [env [_ name fields & specs :as form]]
  (ast :deftype form env
    :name (analyze (expr-env env) name)
    :fields fields))

(defmethod analyze-list 'do [env [_ & body :as form]]
  (ast :do form env
    :body (analyze-block env body)))

(defn analyze-method [env [params & body :as form]]
  (assert (every? symbol? params))
  (let [variadic? (some #{'&} params)
        params (vec (remove #{'&} params))
        env (-> env (update :locals concat params) (assoc :context :return))]
    (ast :fn-method form env
      :params params
      :variadic? variadic?
      :fixed-arity (count (if variadic? (butlast params) params))
      :body (analyze-block env body))))

(defmethod analyze-list 'fn* [env [_ & more :as form]]
  (let [[local more] (if (symbol? (first more))
                       [(first more) (rest more)] [nil more])
        methods (map (partial analyze-method env)
                     (if (vector? (first more)) (list more) more))
        num-variadic-methods (count (filter :variadic? methods))]
    (assert (<= num-variadic-methods 1)
      "Only one variadic method allowed per function.")
    (ast :fn form env
      :local (or local (gensym "fn_"))
      :max-fixed-arity (apply max (map :fixed-arity methods))
      :methods methods
      :variadic? (> num-variadic-methods 0))))

(defmethod analyze-list 'if [env [_ test then else :as form]]
  (ast :if form env
    :test (analyze (expr-env env) test)
    :then (analyze env then)
    :else (analyze env else)))

(defn analyze-bindings [env bindings]
  (assert (vector? bindings))
  (assert (even? (count bindings)))
  (loop [body-env env
         analyzed []
         pairs (partition 2 bindings)]
    (if-let [[bsym bval] (first pairs)]
      (do (assert (symbol? bsym))
          (recur (update body-env :locals conj bsym)
                 (conj analyzed [bsym (analyze (expr-env body-env) bval)])
                 (rest pairs)))
      [body-env analyzed])))

(defmethod analyze-list 'let* [env [_ bindings & body :as form]]
  (let [[body-env bindings] (analyze-bindings env bindings)]
    (ast :let form env
      :bindings bindings
      :body (analyze-block body-env body))))

(defmethod analyze-list 'letfn* [env [_ bindings & body :as form]]
  (let [bsyms (map first bindings)
        body-env (update env :locals concat bsyms)
        fn-asts (map #(analyze (expr-env body-env) (cons 'fn* %)) bindings)]
    (ast :letfn form env
      :bindings (vec (zipmap bsyms fn-asts))
      :body (analyze-block body-env body))))

(defmethod analyze-list 'loop* [env [_ bindings & body :as form]]
  (let [[body-env bindings] (analyze-bindings env bindings)
        body-env (assoc body-env :recur-point {:bindings bindings})]
    (ast :loop form env
      :bindings bindings
      :body (analyze-block body-env body))))

(defmethod analyze-list 'new [env [_ ctor & args :as form]]
  (ast :new form env
    :ctor (analyze (expr-env env) ctor)
    :args (map (partial analyze (expr-env env)) args)))

(defn require-libspec [ns-spec libspec]
  (condp #(%1 %2) libspec
    symbol? (require-ns ns-spec libspec)
    vector? (let [[required & {alias :as, referred :refer}] libspec]
              (-> ns-spec (require-ns required alias)
                          (refer-symbols required referred)))
    (raise "Only vectors and symbols may be used as libspecs." libspec)))

(defn parse-ns-decl [[_ ns-sym & clauses]]
  (reduce (fn [ns-spec [head & libspecs]]
            (assert (= head :require)
              "The ns macro supports only :require at this time.")
            (reduce require-libspec ns-spec libspecs))
          (create-ns-spec (str ns-sym)) clauses))

(defmethod analyze-list 'ns [env form]
  (let [ns-spec (parse-ns-decl form)]
    (swap! state enter-ns ns-spec)
    (ast :ns form env :name (:name ns-spec))))

(defmethod analyze-list 'quote [env [_ expr]]
  (analyze (assoc env :quoted? true) expr))

(defmethod analyze-list 'recur [env [_ & args :as form]]
  (if-let [recur-point (:recur-point env)]
    (ast :recur form env
      :recur-point recur-point
      :args (mapv (partial analyze (expr-env env)) args))
    (raise "May only recur within loop." form)))

(defmethod analyze-list 'set! [env [_ target val :as form]]
  (ast :set! form env
    :target (analyze (expr-env env) target)
    :val (analyze (expr-env env) val)))

(defmethod analyze-list 'throw [env [_ exception :as form]]
  (ast :throw form env
    :exception (analyze (expr-env env) exception)))

;; generic forms

(defn analyze-coll [env form]
  (ast :coll form env
    :children (map (partial analyze (expr-env env)) form)))

(defn analyze-const [env form]
  (ast :const form env))

(defmethod analyze-list :default [env form]
  (if (or (:quoted? env) (empty? form))
    (analyze-coll env form)
    (ast :invoke form env
      :fn (analyze (expr-env env) (first form))
      :args (map (partial analyze (expr-env env)) (rest form)))))

(defn analyze-symbol [env sym]
  (if (or (:quoted? env) (contains? (set (:locals env)) sym))
    (analyze-const env sym)
    (ast :const sym env :form (resolve sym))))

(defn analyze
  ([form] (analyze {:context :statement :locals [] :quoted? false} form))
  ([env form]
    (let [form (cond-> form (not (:quoted? env)) macroexpand)]
      (condp #(%1 %2) form
        seq? (analyze-list env form)
        coll? (analyze-coll env form)
        symbol? (analyze-symbol env form)
        (analyze-const env form)))))
