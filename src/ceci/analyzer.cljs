(ns ceci.analyzer
  (:refer-clojure :exclude [defmacro macroexpand macroexpand-1])
  (:require [ceci.emitter :as emitter]
            [ceci.util :refer [merge-meta raise take-when warn]]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(def ^:dynamic *state* nil)

;; symbol expansion

(defn resolve-ns
  ([ns-name]
    (let [{:keys [current-ns], ns-named :namespaces} @*state*]
      (resolve-ns ns-name (ns-named current-ns))))
  ([ns-name ns-spec]
    (let [{ns-named :namespaces} @*state*]
      (or (ns-named ns-name) (ns-named (get-in ns-spec [:aliases ns-name]))))))

(defn canonicalize
  "Returns the canonical expansion of `sym` in the context of `ns-spec`. Uses
  the currently active namespace if no `ns-spec` is provided."
  ([sym] (canonicalize sym (resolve-ns (:current-ns @*state*))))
  ([sym ns-spec]
    (if-let [ns-name (namespace sym)]
      (if (= ns-name "js")
        sym ; leave e.g. js/foo.bar untouched
        (if-let [required-ns (resolve-ns (symbol ns-name) ns-spec)]
          (if (contains? (:defs required-ns) (symbol (name sym)))
            (symbol (str (:name required-ns)) (name sym))
            (raise "Symbol not defined in required namespace." sym))
          (raise "No such namespace." sym)))
      (if (contains? (:defs ns-spec) sym)
        (symbol (str (:name ns-spec)) (name sym))
        (if-let [referred-ns (get-in ns-spec [:referred sym])]
          (symbol (str referred-ns) (name sym))
          (raise "Symbol not defined in current namespace." sym))))))

(defn define [state sym var]
  (let [ns (or (some-> sym namespace symbol) (:current-ns state))]
    (assoc-in state [:namespaces ns :defs (symbol (name sym))] var)))

;; macroexpansion & syntax desugaring

(defn expand-macro [form]
  (try (let [sym (canonicalize (first form))
             var (get-in @*state* [:namespaces (symbol (namespace sym))
                                   :defs (symbol (name sym))])]
         (if (:macro var) (apply (:value var) (rest form)) form))
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

(defn find-auto-gensyms [form]
  (cond (coll? form) (apply set/union (map find-auto-gensyms form))
        (and (symbol? form)
             (not (namespace form))
             (= (last (name form)) \#)) #{form}
        :else #{}))

(defn desugar-auto-gensym [sym]
  (let [s (name sym)]
    (symbol (str (gensym (str (subs s 0 (dec (count s))) "__")) "__auto__"))))

(defn desugar-auto-gensyms
  "Replaces auto-gensyms (e.g. `foo#`) with gensyms (e.g. `foo__1000__auto__`)
  within `form`. Part of the standard behavior of `syntax-quote`."
  [form]
  (let [syms (find-auto-gensyms form)]
    (walk/postwalk-replace (zipmap syms (map desugar-auto-gensym syms)) form)))

;; core defs

(def core-functions
  '[+ - * / = > >= < <= aget and apply aset assoc assoc-in atom boolean comp
    concat conj cons constantly dec dissoc empty? filter first fnil gensym get
    get-in hash hash-map identity inc interleave interpose into juxt key keys
    keyword keyword? list list? map map? merge nil? not not= number? or partial
    partition print println pr prn pr-str reduce remove reset! rest reverse
    second seq seq? set set? str swap! update-in val vals vec vector vector?])

(defn defmacro [name & more]
  `(def ~(with-meta name {:macro true}) (fn* ~@more)))

(defn syntax-quote [form]
  (let [form (desugar-auto-gensyms form)
        unquote? (every-pred list? (comp #{'unquote} first))
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

(def core-macros
  {'defmacro defmacro, 'syntax-quote syntax-quote})

(def core-defs
  (into {} (concat (for [fname core-functions]
                     [fname {:ns 'cljs.core :name fname}])
                   (for [[mname macro] core-macros]
                     [mname {:ns 'cljs.core :name mname
                             :value macro :macro true}]))))

;; namespace management

(defn require-ns
  ([ns-spec required]
    (require-ns ns-spec required nil))
  ([ns-spec required alias]
    (update ns-spec :aliases merge {alias (or alias required)})))

(defn refer-symbols [ns-spec from-ns syms]
  (update ns-spec :referred merge (zipmap syms (repeat from-ns))))

(defn create-ns-spec [ns-name]
  (-> {:name ns-name :defs {}}
      (require-ns 'cljs.core) (refer-symbols 'cljs.core (keys core-defs))))

(defn enter-ns [state {:keys [name] :as ns-spec}]
  (-> state (assoc-in [:namespaces name] ns-spec) (assoc :current-ns name)))

(defn default-state []
  (atom (-> {:current-ns nil
             :namespaces {'cljs.core {:name 'cljs.core :defs core-defs}}}
            (enter-ns (create-ns-spec 'user)))))

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

(defn ast
  "Given an `op` keyword, a Clojure `form`, an `env`, and zero or more keyword
  arguments `attrs`, returns an AST node with the desired properties."
  [op form env & attrs]
  (merge {:op op :form form :env env :meta (meta form) :type (node-type form)}
         (apply hash-map attrs)))

(declare analyze)

(defn ctx
  "Returns a copy of `env` with :context set to `ctx`."
  [env ctx]
  (assoc env :context ctx))

(defn analyze-block [env forms]
  (let [body-env (ctx env :statement)
        return-env (update env :context #(if (= % :statement) % :return))]
    (conj (mapv (partial analyze body-env) (butlast forms))
          (analyze return-env (last forms)))))

(defmulti analyze-special (fn [_ form] (first form)))

;; special forms

(defn analyze-var [env sym]
  (ast :var sym env :name (symbol (name sym)) :ns (:current-ns @*state*)))

(defn warn-def-shadows-referral [sym form]
  (when-let [ns (get-in @*state* [:namespaces (:current-ns @*state*) :referred sym])]
    (warn (str "Def shadows existing referral: " ns "/" sym) form)))

(defmethod analyze-special 'def
  [env [_ name init :as form]]
  (warn-def-shadows-referral name form)
  (swap! *state* define name {}) ; HACK: ensure name is bound when analyzing init
  (let [init (analyze (ctx env :expr) init)
        var (assoc (analyze-var env name) :init init)
        var (cond-> var (:macro (:meta var))
              (assoc :macro true
                     :value (js/eval (str "(" (emitter/emit init) ")"))))]
    (swap! *state* define name var)
    (ast :def form env :name name :var var :init init)))

(defmethod analyze-special 'deftype*
  [env [_ name fields & specs :as form]]
  (warn-def-shadows-referral name form)
  (let [var (analyze-var env name)]
    (swap! *state* define name var)
    (ast :deftype form env :name name :var var :fields fields)))

(defmethod analyze-special 'do
  [env [_ & body :as form]]
  (ast :do form env
    :body (analyze-block env body)))

(defn analyze-local [env sym]
  (ast :local sym env :name (gensym (name sym))))

(defn recursive? [form]
  (when (seq? form)
    (condp contains? (first form)
      '#{recur} true
      '#{do let* letfn*} (recursive? (last form))
      '#{if} (some recursive? (take-last 2 form))
      false)))

(defn rewrite-recursive-fn-method [params body variadic?]
  (let [gensyms (interleave params (repeatedly gensym))
        arglist (map second (partition 2 gensyms))
        arglist (vec (if variadic?
                       (concat (butlast arglist) ['& (last arglist)])
                       arglist))]
    `(~arglist (loop* [~@gensyms] ~@body))))

(defn analyze-fn-method [env [params & body :as form]]
  (assert (every? symbol? params))
  (let [variadic? (some #{'&} params)
        params (vec (remove #{'&} params))]
    (if (recursive? (last body))
      (recur env (rewrite-recursive-fn-method params body variadic?))
      (let [locals (zipmap params (map (partial analyze-local env) params))
            env (-> env (update :locals merge locals) (ctx :return))]
        (ast :fn-method form env
          :params (map locals params)
          :variadic? variadic?
          :fixed-arity (count (if variadic? (butlast params) params))
          :body (analyze-block env body))))))

(defmethod analyze-special 'fn*
  [env [_ & more :as form]]
  (let [[local more] (take-when symbol? more)
        methods (map (partial analyze-fn-method env)
                     (if (vector? (first more)) (list more) more))
        num-variadic-methods (count (filter :variadic? methods))]
    (assert (<= num-variadic-methods 1)
      "Only one variadic method allowed per function.")
    (ast :fn form env
      :local (or local (gensym "fn_"))
      :max-fixed-arity (apply max (map :fixed-arity methods))
      :methods methods
      :variadic? (> num-variadic-methods 0))))

(defmethod analyze-special 'if
  [env [_ test then else :as form]]
  (ast :if form env
    :test (analyze (ctx env :expr) test)
    :then (analyze env then)
    :else (analyze env else)))

(defn analyze-bindings [env bvec]
  (assert (vector? bvec))
  (assert (even? (count bvec)))
  (reduce (fn [[env bindings] [bsym bval]]
            (let [init (analyze (ctx env :expr) bval)
                  local (assoc (analyze-local (ctx env :expr) bsym) :init init)]
              [(assoc-in env [:locals bsym] local)
               (conj bindings (assoc local :op :binding))]))
          [env []] (partition 2 bvec)))

(defmethod analyze-special 'let*
  [env [_ bvec & body :as form]]
  (let [[body-env bindings] (analyze-bindings env bvec)]
    (ast :let form env
      :bindings bindings
      :body (analyze-block body-env body))))

(defmethod analyze-special 'letfn*
  [env [_ bvec & body :as form]]
  (let [bsyms (map first bvec)
        locals (zipmap bsyms (map (partial analyze-local env) bsyms))
        body-env (update env :locals merge locals)
        inits (map #(analyze (ctx body-env :expr) (cons 'fn* %)) bvec)]
    (ast :letfn form env
      :bindings (map #(assoc %1 :op :binding :init %2) (vals locals) inits)
      :body (analyze-block body-env body))))

(defmethod analyze-special 'loop*
  [env [_ bvec & body :as form]]
  (let [[body-env bindings] (analyze-bindings env bvec)
        body-env (assoc body-env :recur-point {:bindings bindings})]
    (ast :loop form env
      :bindings bindings
      :body (analyze-block body-env body))))

(defmethod analyze-special 'new
  [env [_ ctor & args :as form]]
  (ast :new form env
    :ctor (analyze (ctx env :expr) ctor)
    :args (map (partial analyze (ctx env :expr)) args)))

(defn require-libspec [ns-spec libspec]
  (condp #(%1 %2) libspec
    symbol? (require-ns ns-spec libspec)
    vector? (let [[required & {alias :as, referred :refer}] libspec]
              (-> ns-spec (require-ns required alias)
                          (refer-symbols required referred)))
    (raise "Only vectors and symbols may be used as libspecs." libspec)))

(defn exclude-core-defs [ns-spec exclusions]
  (let [excluded? (comp (partial contains? (set exclusions)) key)
        core-def? (comp (partial = 'cljs.core) val)]
    (update ns-spec :referrals
            #(into {} (remove (every-pred excluded? core-def?) %)))))

(defn rename-core-defs [ns-spec renames]
  (update ns-spec :referrals
          #(into {} (map (fn [[sym ns :as entry]]
                           (if (= ns 'cljs.core)
                             [(renames sym sym) ns] entry)) %))))

(defn refer-clojure [ns-spec opts]
  (let [{:keys [exclude rename] :or {exclude [] rename {}}} opts]
    (assert (sequential? exclude))
    (assert (map? rename))
    (-> ns-spec (exclude-core-defs exclude) (rename-core-defs rename))))

(defn parse-ns-clause [ns-spec [head & more :as form]]
  (case head
    :require (reduce require-libspec ns-spec more)
    :refer-clojure (refer-clojure ns-spec (apply hash-map more))
    (raise "The ns macro supports only :require and :refer-clojure." form)))

(defn parse-ns-decl [[_ ns-sym & clauses]]
  (reduce parse-ns-clause (create-ns-spec ns-sym) clauses))

(defmethod analyze-special 'ns
  [env form]
  (let [ns-spec (parse-ns-decl form)]
    (swap! *state* enter-ns ns-spec)
    (ast :ns form env :name (:name ns-spec))))

(defmethod analyze-special 'quote
  [env [_ expr]]
  (analyze (assoc env :quoted? true) expr))

(defmethod analyze-special 'recur
  [env [_ & args :as form]]
  (if-let [recur-point (:recur-point env)]
    (let [expected (count (:bindings recur-point))
          actual (count args)]
      (when-not (= expected actual)
        (raise (str "Bad argument count to recur: expected " expected
                    ", got " actual) form))
      (ast :recur form env
        :recur-point recur-point
        :args (mapv (partial analyze (ctx env :expr)) args)))
    (raise "May only recur within loop." form)))

(defmethod analyze-special 'set!
  [env [_ target val :as form]]
  (ast :set! form env
    :target (analyze (ctx env :expr) target)
    :val (analyze (ctx env :expr) val)))

(defmethod analyze-special 'throw
  [env [_ exception :as form]]
  (ast :throw form env
    :exception (analyze (ctx env :expr) exception)))

;; generic forms

(defn analyze-coll [env form]
  (let [analyze* (partial analyze (ctx env :expr))]
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
    (:binding :local)
      (or (resolve-ref (:init ast)) ast)
    :var
      (let [var (get-in @*state* [:namespaces (:ns ast) :defs (:name ast)])]
        (or (resolve-ref (:init var)) ast))
    ;else
      ast))

(defn valid-invoke-arity?
  "Returns truthy if it is acceptable to invoke `f` with `argc` arguments."
  [f argc]
  (case (:op f)
    (:coll :const)
      (and (#{:keyword :map :set :symbol :vector} (:type f))
           (#{1 2} argc))
    :fn
      (or (and (:variadic? f) (>= argc (:max-fixed-arity f)))
          (some #(= (:fixed-arity %) argc) (:methods f)))
    true)) ; we're forced to assume any other kind of expr is always invokable

(defn analyze-invoke [env form]
  (let [func (analyze (ctx env :expr) (first form))
        args (map (partial analyze (ctx env :expr)) (rest form))]
    (when-not (valid-invoke-arity? (resolve-ref func) (count args))
      (raise (str "Invalid arity: " (count args)) form))
    (ast :invoke form env :fn func :args args)))

(defn analyze-list [env form]
  (if (or (:quoted? env) (empty? form))
    (analyze-coll env form)
    (if-let [analyze-special* (get-method analyze-special (first form))]
      (analyze-special* env form)
      (analyze-invoke env form))))

(defn analyze-symbol [env sym]
  (if (:quoted? env)
    (analyze-const env sym)
    (if-let [local (get-in env [:locals sym])]
      (update local :env ctx (:context env))
      (if (= (namespace sym) "js")
        (ast :js-var sym env)
        (let [sym (canonicalize sym)]
          (ast :var sym env
            :ns (symbol (namespace sym)) :name (symbol (name sym))))))))

(defn analyze
  ([form] (analyze {:context :statement :locals {} :quoted? false} form))
  ([env form]
    (let [form (cond-> form (not (:quoted? env)) macroexpand)]
      (condp #(%1 %2) form
        seq? (analyze-list env form)
        coll? (analyze-coll env form)
        symbol? (analyze-symbol env form)
        (analyze-const env form)))))
