(ns clueless.analyzer
  (:require [clueless.emitter :as emitter]
            [clueless.expander :as expander]
            [clueless.util :refer [raise update]]))

;; AST creation

(defn node-type [form]
  (cond (keyword? form) :keyword
        (list? form) :list
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
        (-> ast (assoc :op :coll) (assoc :children (map form->ast form)))
        (-> ast (assoc :op :const)))))

;; AST analysis

(declare analyze)

(def true-ast-node
  {:op :const :type :bool :form true})

(def false-ast-node
  {:op :const :type :bool :form false})

(def nil-ast-node
  {:op :const :type :nil :form nil})

;; aget, aset (interop) forms

(defn analyze-aget [env {[_ target & fields] :children :as ast}]
  (-> ast
      (assoc :op :aget)
      (assoc :target (analyze env target))
      (assoc :fields (map (partial analyze env) fields))))

(defn analyze-aset [env {[_ target & fields+value] :children :as ast}]
  (let [fields (drop-last fields+value)
        value (last fields+value)]
    (-> ast
        (assoc :op :aset)
        (assoc :target (analyze env target))
        (assoc :fields (map (partial analyze env) fields))
        (assoc :value (analyze env value)))))

;; def, do, if forms

(defn analyze-def [env {[_ name & [init?]] :children :as ast}]
  (-> ast
      (assoc :op :def)
      (assoc :name (analyze env name))
      (assoc :init (analyze env (or init? nil-ast-node)))))

(defn analyze-do [env {[_ & body] :children :as ast}]
  (-> ast
      (assoc :op :do)
      (assoc :body (map (partial analyze env) body))))

(defn analyze-if [env {[_ test then else] :children :as ast}]
  (-> ast
      (assoc :op :if)
      (assoc :test (analyze env test))
      (assoc :then (analyze env then))
      (assoc :else (analyze env else))))

(defn analyze-new [env {[_ ctor & args] :children :as ast}]
  (-> ast
      (assoc :op :new)
      (assoc :ctor (analyze env ctor))
      (assoc :args (map (partial analyze env) args))))

(defn analyze-quote [env {[_ ast] :children}]
  (let [env (assoc env :quoted? true)]
    (analyze env ast)))

(defn analyze-throw [env {[_ thrown] :children :as ast}]
  (-> ast
      (assoc :op :throw)
      (assoc :thrown (analyze env thrown))))

;; fn forms

(defn analyze-params [env {:keys [children]}]
  (let [params (vec (map :form children))]
    [(update env :locals concat params) params]))

(defn analyze-clauses [env clauses]
  (loop [analyzed {} env env clauses clauses]
    (if-let [[params & body] (first clauses)]
      (let [[env params] (analyze-params env params)
            clause {:params params :body (map (partial analyze env) body)}]
        (recur (assoc analyzed (count params) clause) env (rest clauses)))
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
  (let [clauses (extract-clauses ast)]
    (-> ast
        (assoc :op :fn)
        (assoc :clauses (analyze-clauses env clauses)))))

;; defmacro forms

(defn analyze-defmacro [env {[_ name-node & _] :children :as ast}]
  (let [macro-name (:form name-node)
        macro-node (analyze-fn env ast)]
    (expander/install-macro! macro-name (js/eval (emitter/emit macro-node)))
    macro-node))

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
             (conj analyzed [left-hand (analyze env right-hand)])
             (inc idx))
      [env analyzed])))

(defn analyze-let [env {[_ bindings & body] :children :as ast}]
  (let [[env bindings] (analyze-bindings env (compile-bindings bindings))]
    (-> ast
        (assoc :op :let)
        (assoc :bindings bindings)
        (assoc :body (map (partial analyze env) body)))))

;; generic interface

(defn analyze-coll [env ast]
  (-> ast
      (assoc :env env)
      (update :children #(map (partial analyze env) %))))

(def specials
  {'aget analyze-aget
   'aset analyze-aset
   'def analyze-def
   'defmacro analyze-defmacro
   'do analyze-do
   'fn analyze-fn
   'if analyze-if
   'let analyze-let
   'new analyze-new
   'quote analyze-quote
   'throw analyze-throw})

(defn analyze-list [env {:keys [form] :as ast}]
  (let [analyze-special (specials (first form))]
    (if (and analyze-special (not (:quoted? env)))
        (analyze-special env ast)
        (analyze-coll env ast))))

(defn analyze-symbol [env {sym :form :as ast}]
  (let [ast (assoc ast :env env)]
    (cond (:quoted? env) ast
          (= sym (symbol "true")) true-ast-node
          (= sym (symbol "false")) false-ast-node
          (= sym (symbol "nil")) nil-ast-node
          ((set (:locals env)) sym) ast
          :else (assoc ast :form (clueless.env/resolve sym)))))

(defn analyze
  ([ast] (analyze {:locals [] :quoted? false} ast))
  ([env {:keys [op type] :as ast}]
    (cond (= type :list) (analyze-list env ast)
          (= op :coll) (analyze-coll env ast)
          (= type :symbol) (analyze-symbol env ast)
          :else ast)))
