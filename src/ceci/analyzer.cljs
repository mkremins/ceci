(ns ceci.analyzer
  (:refer-clojure :exclude [defmacro macroexpand macroexpand-1])
  (:require [ceci.emitter :as emitter]
            [ceci.util :refer [merge-meta raise update warn]]))

(def ^:dynamic *state* nil)

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
  (-> {:name ns-name :defs {}}
      (require-ns "cljs.core") (refer-symbols "cljs.core" core-defs)))

(defn enter-ns [state {:keys [name] :as ns-spec}]
  (-> state (assoc-in [:namespaces name] ns-spec) (assoc :current-ns name)))

;; symbol expansion

(defn resolve-ns
  ([ns-name]
    (let [{:keys [current-ns namespaces]} @*state*]
      (resolve-ns ns-name (namespaces current-ns))))
  ([ns-name ns-spec]
    (let [{:keys [namespaces]} @*state*]
      (or (namespaces ns-name)
          (when-let [ns-name* (get-in ns-spec [:aliases ns-name])]
            (namespaces ns-name*))))))

(defn canonicalize
  "Returns the canonical expansion of `sym` in the context of `ns-spec`. Uses
  the currently active namespace if no `ns-spec` is provided."
  ([sym] (canonicalize sym (resolve-ns (:current-ns @*state*))))
  ([sym ns-spec]
    (if-let [ns-name (namespace sym)]
      (if (= ns-name "js")
        sym ; leave e.g. js/foo.bar untouched
        (if-let [required-ns (:name (resolve-ns ns-name ns-spec))]
          (symbol required-ns (name sym))
          (raise "No such namespace." sym)))
      (if (contains? (:defs ns-spec) sym)
        (symbol (:name ns-spec) (name sym))
        (if-let [referred-ns (get-in ns-spec [:referred sym])]
          (symbol referred-ns (name sym))
          (raise "Symbol not defined in current namespace." sym))))))

(defn define
  ([state sym def-ast] (define state (:current-ns state) sym def-ast))
  ([state ns-name sym def-ast]
    (assoc-in state [:namespaces ns-name :defs sym] def-ast)))

;; macroexpansion & syntax desugaring

(defn expand-macro [form]
  (try (let [sym (canonicalize (first form))
             def (get-in @*state* [:namespaces (namespace sym)
                                   :defs (symbol (name sym))])]
         (if (:macro def) (apply (:value def) (rest form)) form))
       (catch :default _ form)))

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

;; core macros

(defn defmacro [name & more]
  `(def ~(with-meta name {:macro true}) (fn* ~@more)))

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

;; AST creation

(defn default-state []
  (doto (atom {:current-ns nil :namespaces {}})
    (swap! enter-ns (create-ns-spec "user"))
    (swap! define "cljs.core" 'defmacro
      {:op :def :macro true :value defmacro})
    (swap! define "cljs.core" 'syntax-quote
      {:op :def :macro true :value syntax-quote})))

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

(defn analyze-var [env sym]
  (ast :var (symbol (:current-ns @*state*) (name sym)) env))

(defn warn-def-shadows-referral [sym form]
  (when-let [ns (get-in @*state* [:namespaces (:current-ns @*state*) :referred sym])]
    (warn (str "Def shadows existing referral: " ns "/" sym) form)))

(defmethod analyze-list 'def [env [_ name init :as form]]
  (warn-def-shadows-referral name form)
  (swap! *state* define name {}) ; HACK: ensure name is bound when analyzing init
  (let [init (analyze (expr-env env) init)
        def (merge (ast :def form env
                     :name name :var (analyze-var env name) :init init)
                   (select-keys (meta name) [:doc :dynamic :macro :private])
                   (when (:macro (meta name))
                     {:value (js/eval (str "(" (emitter/emit init) ")"))}))]
    (swap! *state* define name def)
    def))

(defmethod analyze-list 'deftype* [env [_ name fields & specs :as form]]
  (warn-def-shadows-referral name form)
  (let [def (ast :deftype form env
              :name name :var (analyze-var env name) :fields fields)]
    (swap! *state* define name def)
    def))

(defmethod analyze-list 'do [env [_ & body :as form]]
  (ast :do form env
    :body (analyze-block env body)))

(defn analyze-local [env sym]
  (ast :local sym env :name (gensym (name sym))))

(defn analyze-method [env [params & body :as form]]
  (assert (every? symbol? params))
  (let [variadic? (some #{'&} params)
        params (vec (remove #{'&} params))
        locals (zipmap params (map (partial analyze-local env) params))
        env (-> env (update :locals merge locals) (assoc :context :return))]
    (ast :fn-method form env
      :params (map locals params)
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
  (loop [env env
         analyzed []
         bpairs (partition 2 bindings)]
    (if-let [[bsym bval] (first bpairs)]
      (let [init (analyze (expr-env env) bval)
            local (assoc (analyze-local (expr-env env) bsym) :init init)]
        (recur (assoc-in env [:locals bsym] local)
               (conj analyzed [local init]) (rest bpairs)))
      [env analyzed])))

(defmethod analyze-list 'let* [env [_ bindings & body :as form]]
  (let [[body-env bindings] (analyze-bindings env bindings)]
    (ast :let form env
      :bindings bindings
      :body (analyze-block body-env body))))

(defmethod analyze-list 'letfn* [env [_ bindings & body :as form]]
  (let [bsyms (map first bindings)
        locals (zipmap bsyms (map (partial analyze-local env) bsyms))
        body-env (update env :locals merge locals)
        fn-asts (map #(analyze (expr-env body-env) (cons 'fn* %)) bindings)]
    (ast :letfn form env
      :bindings (vec (zipmap (map locals bsyms) fn-asts))
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
    (swap! *state* enter-ns ns-spec)
    (ast :ns form env :name (:name ns-spec))))

(defmethod analyze-list 'quote [env [_ expr]]
  (analyze (assoc env :quoted? true) expr))

(defmethod analyze-list 'recur [env [_ & args :as form]]
  (if-let [recur-point (:recur-point env)]
    (let [expected (count (:bindings recur-point))
          actual (count args)]
      (when-not (= expected actual)
        (raise (str "Bad argument count to recur: expected " expected
                    ", got " actual) form))
      (ast :recur form env
        :recur-point recur-point
        :args (mapv (partial analyze (expr-env env)) args)))
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
  (let [analyze* (partial analyze (expr-env env))]
    (if (map? form)
      (ast :coll form env
        :keys (map analyze* (keys form))
        :vals (map analyze* (vals form)))
      (ast :coll form env
        :items (map analyze* form)))))

(defn analyze-const [env form]
  (ast :const form env))

(defn resolve-ref [ast]
  (case (:op ast)
    :local (or (resolve-ref (:init ast)) ast)
    :var (let [ns-name (namespace (:form ast))
               sym (symbol (name (:form ast)))
               def (get-in @*state* [:namespaces ns-name :defs sym])]
           (or (resolve-ref (:init def)) ast))
    ast))

(defn valid-invoke-arity? [f arity]
  (case (:op f)
    (:coll :const)
      (and (#{:keyword :map :set :symbol :vector} (:type f))
           (#{1 2} arity))
    :fn
      (or (and (:variadic? f) (>= arity (:max-fixed-arity f)))
          (some #(= (:fixed-arity %) arity) (:methods f)))
    ;else
      true))

(defmethod analyze-list :default [env form]
  (if (or (:quoted? env) (empty? form))
    (analyze-coll env form)
    (let [func (analyze (expr-env env) (first form))
          args (map (partial analyze (expr-env env)) (rest form))]
      (when-not (valid-invoke-arity? (resolve-ref func) (count args))
        (raise (str "Invalid arity: " (count args)) form))
      (ast :invoke form env :fn func :args args))))

(defn analyze-symbol [env sym]
  (if (:quoted? env)
    (analyze-const env sym)
    (if-let [local (get-in env [:locals sym])]
      (assoc-in local [:env :context] (:context env))
      (if (= (namespace sym) "js")
        (ast :js-var sym env)
        (ast :var (canonicalize sym) env)))))

(defn analyze
  ([form] (analyze {:context :statement :locals {} :quoted? false} form))
  ([env form]
    (let [form (cond-> form (not (:quoted? env)) macroexpand)]
      (condp #(%1 %2) form
        seq? (analyze-list env form)
        coll? (analyze-coll env form)
        symbol? (analyze-symbol env form)
        (analyze-const env form)))))
