(ns ceci.generator
  (:refer-clojure :exclude [array])
  (:require [ceci.util :refer [in?]]
            [clojure.string :as string]))

;; helpers

(declare generate)

(def escodegen (js/require "escodegen"))

(defn escape [s]
  (-> (name s)
      (string/replace #"\+" "_PLUS_")
      (string/replace #"-" "_")
      (string/replace #"\*" "_STAR_")
      (string/replace #"/" "_SLASH_")
      (string/replace #"\?" "_QMARK_")
      (string/replace #"!" "_BANG_")
      (string/replace #"<" "_LT_")
      (string/replace #">" "_GT_")
      (string/replace #"=" "_EQ_")))

(defn array [elements]
  {:type "ArrayExpression" :elements elements})

(defn literal [value]
  {:type "Literal" :value value})

(defn identifier [name]
  {:type "Identifier" :name name})

(defn statement
  "If the Escodegen-compatible JavaScript AST node `expr-or-statement` is a
  statement node, returns it verbatim. Otherwise, coerces it to a statement
  node by wrapping it in an ExpressionStatement node and returns the result."
  [{:keys [type] :as expr-or-statement}]
  (if (re-find #"Statement" type)
      expr-or-statement
      {:type "ExpressionStatement"
       :expression expr-or-statement}))

(defn assign [left right]
  {:type "AssignmentExpression" :operator "=" :left left :right right})

(defn generate-bindings
  ([bindings] (generate-bindings bindings []))
  ([bindings locals]
    (map (fn [[k v]]
           (assign (identifier (escape k))
                   (if (in? locals v)
                       (identifier (escape v))
                       (generate v))))
         bindings)))

;; specials

(defmulti generate-special :op)

(defmethod generate-special :aget [{:keys [target fields]}]
  (reduce (fn [js-ast field]
            {:type "MemberExpression"
             :object js-ast
             :property (generate field)
             :computed true})
          (generate target) fields))

(defmethod generate-special :aset [{:keys [value] :as ast}]
  (assign (generate-special (assoc ast :op :aget)) (generate value)))

(defmethod generate-special :def [{:keys [name init]}]
  (assign (generate name) (generate init)))

(defmethod generate-special :deftype [{:keys [name fields]}]
  (letfn [(assign-field [field]
            (let [fname (identifier (escape field))]
              (assign {:type "MemberExpression"
                       :object {:type "ThisExpression"}
                       :property fname :computed false}
                      fname)))]
    (assign (generate name)
            {:type "FunctionExpression"
             :params (map (comp identifier escape) fields)
             :body {:type "BlockStatement"
                    :body (map (comp statement assign-field) fields)}})))

(defmethod generate-special :do [{:keys [body]}]
  {:type "BlockStatement"
   :body (map (comp statement generate) body)})

(defmethod generate-special :if [{:keys [env test then else]}]
  (let [base {:test (generate test)
              :consequent (statement (generate then))
              :alternate (statement (generate else))}
        type (if (= (:context env) :expr)
                 "ConditionalExpression"
                 "IfStatement")]
    (merge base {:type type})))

(defmethod generate-special :invoke [{:keys [invoked args]}]
  {:type "CallExpression"
   :callee {:type "MemberExpression"
            :object (generate invoked)
            :property (identifier "call")
            :computed false}
   :arguments (concat [(literal nil)] (map generate args))})

(defmethod generate-special :let [{:keys [bindings body]}]
  {:type "BlockStatement"
   :body (map statement
              (concat (when (seq bindings) (generate-bindings bindings))
                      (map generate body)))})

(defmethod generate-special :loop [{:keys [bindings body env]}]
  (let [base
        {:type "WhileStatement" :test (literal true)
         :body {:type "BlockStatement"
                :body (map statement
                           (concat (when (seq bindings)
                                     (generate-bindings bindings))
                                   (map generate body)
                                   [{:type "BreakStatement"}]))}}]
    (if (not= (:context env) :statement)
        {:type "CallExpression"
         :callee {:type "FunctionExpression"
                  :body {:type "BlockStatement" :body [base]}
                  :params []}
         :arguments []}
        base)))

(defmethod generate-special :new [{:keys [ctor args]}]
  {:type "NewExpression"
   :callee (generate ctor)
   :arguments (map generate args)})

(defmethod generate-special :recur [{:keys [args recur-point]}]
  (let [rebinds (vec (map first (:bindings recur-point)))
        num-args (count args)
        temps (vec (take num-args (repeatedly gensym)))
        bindings (concat (map (juxt temps args) (range num-args))
                         (map (juxt rebinds temps) (range num-args)))]
    {:type "BlockStatement"
     :body (map statement (concat (generate-bindings bindings temps)
                          [{:type "ContinueStatement"}]))}))

(defmethod generate-special :throw [{:keys [env thrown]}]
  {:type "ThrowStatement"
   :argument (generate thrown)})

;; collections

(defmulti generate-collection :type)

(defmethod generate-collection :list [{:keys [children]}]
  (if (empty? children)
      (identifier "cljs.core.List.EMPTY")
      {:type "CallExpression"
       :callee (identifier "cljs.core.list")
       :arguments (map generate children)}))

(defmethod generate-collection :vector [{:keys [children]}]
  (if (empty? children)
      (identifier "cljs.core.PersistentVector.EMPTY")
      {:type "CallExpression"
       :callee (identifier "cljs.core.PersistentVector.fromArray")
       :arguments [(array (map generate children)) (literal true)]}))

(defmethod generate-collection :map [{:keys [children]}]
  (if (empty? children)
      (identifier "cljs.core.PersistentArrayMap.EMPTY")
      (let [pairs (map :children children)
            ks (map first pairs)
            vs (map second pairs)]
        {:type "NewExpression"
         :callee (identifier "cljs.core.PersistentArrayMap.fromArray")
         :arguments [(array (map generate (interleave ks vs)))
                     (literal true) (literal false)]})))

(defmethod generate-collection :set [{:keys [children]}]
  (if (empty? children)
      (identifier "cljs.core.PersistentHashSet.EMPTY")
      {:type "CallExpression"
       :callee (identifier "cljs.core.PersistentHashSet.fromArray")
       :arguments [(array (map generate children)) (literal true)]}))

;; constants

(defmulti generate-constant :type)

(defmethod generate-constant :keyword [{:keys [form]}]
  {:type "NewExpression"
   :callee (identifier "cljs.core.Keyword")
   :arguments (map literal [nil (name form) (name form) (hash form)])})

(defmethod generate-constant :number [{:keys [form]}]
  (if (neg? form) ; Escodegen chokes on negative numbers, because JavaScript
      {:type "UnaryExpression"
       :operator "-" :prefix true
       :argument (literal (* -1 form))}
      (literal form)))

(defmethod generate-constant :symbol [{:keys [form] {:keys [quoted?]} :env}]
  (let [name (name form) ns (namespace form)]
    (if quoted?
        {:type "NewExpression"
         :callee (identifier "cljs.core.Symbol")
         :arguments (map literal
                         [ns name (str (when ns (str ns ".")) name)
                          (hash form) nil])}
        (identifier (str (when (and ns (not= ns "js"))
                           (str (escape ns) "."))
                         (escape name))))))

(defmethod generate-constant :default [{:keys [form]}]
  (literal form))

;; generic

(def block? #{:do :if :let :loop :recur})

(defn generate
  "Given an AST node `ast`, returns an equivalent JavaScript AST compatible
  with the SpiderMonkey Parser API (and, thus, Escodegen)."
  [{:keys [op] {:keys [context]} :env :as ast}]
  (let [js-ast (condp = op
                 :const (generate-constant ast)
                 :coll (generate-collection ast)
                 (generate-special ast))]
    (if (and (= context :return) (not (block? op)))
        {:type "ReturnStatement" :argument js-ast}
        js-ast)))

(defn emit [ast]
  (.generate escodegen (clj->js (generate ast))))
