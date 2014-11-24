(ns ceci.emitter
  (:refer-clojure :exclude [array])
  (:require [clojure.string :as str]))

;; helpers

(declare generate)

(def escodegen (js/require "escodegen"))

(defn munge [s]
  (-> (name s)
      (str/replace #"\+" "_PLUS_")
      (str/replace #"-" "_")
      (str/replace #"\*" "_STAR_")
      (str/replace #"/" "_SLASH_")
      (str/replace #"\?" "_QMARK_")
      (str/replace #"!" "_BANG_")
      (str/replace #"<" "_LT_")
      (str/replace #">" "_GT_")
      (str/replace #"=" "_EQ_")))

(defn array [elements]
  {:type :ArrayExpression :elements elements})

(defn literal [value]
  (if (and (number? value) (neg? value))
    ;; Escodegen chokes on negative number literals, because JavaScript
    {:type :UnaryExpression
     :operator "-" :prefix true
     :argument (literal (* -1 value))}
    {:type :Literal :value value}))

(defn identifier [name]
  {:type :Identifier :name name})

(defn statement
  "If the Escodegen-compatible JavaScript AST node `expr-or-stmt` is a
  statement node, returns it verbatim. Otherwise, coerces it to a statement
  node by wrapping it in an ExpressionStatement node and returns the result."
  [expr-or-stmt]
  (if (re-find #"Statement|Declaration" (name (:type expr-or-stmt)))
    expr-or-stmt
    {:type :ExpressionStatement :expression expr-or-stmt}))

(defn assign [left right]
  {:type :AssignmentExpression :operator "=" :left left :right right})

(defn block
  "Wraps `nodes`, zero or more AST nodes or sequences of AST nodes, in a single
  BlockStatement node, flattening recursive BlockStatements."
  [& nodes]
  {:type :BlockStatement
   :body (->> (mapcat #(if (or (sequential? %) (nil? %)) % [%]) nodes)
              (map statement)
              (mapcat #(if (= (:type %) :BlockStatement) (:body %) [%])))})

;; specials

(defmulti generate-special :op)

(defmethod generate-special :def [{:keys [name init]}]
  (assign (generate name) (generate init)))

(defmethod generate-special :deftype [{:keys [name fields]}]
  (letfn [(assign-field [field]
            (let [fname (identifier (munge field))]
              (assign {:type :MemberExpression
                       :object {:type :ThisExpression}
                       :property fname :computed false}
                      fname)))]
    (assign (generate name)
            {:type :FunctionExpression
             :params (map (comp identifier munge) fields)
             :body (block (map assign-field fields))})))

(defmethod generate-special :do [{:keys [body]}]
  (block (map generate body)))

(defmethod generate-special :if [{:keys [env test then else]}]
  {:type (if (= (:context env) :expr) :ConditionalExpression :IfStatement)
   :test (generate test)
   :consequent (statement (generate then))
   :alternate (statement (generate else))})

(defmethod generate-special :invoke [{:keys [args], target :fn}]
  {:type :CallExpression
   :callee {:type :MemberExpression
            :object (generate target)
            :property (identifier "call")
            :computed false}
   :arguments (concat [(literal nil)] (map generate args))})

(defmethod generate-special :new [{:keys [ctor args]}]
  {:type :NewExpression
   :callee (generate ctor)
   :arguments (map generate args)})

(defmethod generate-special :throw [{:keys [env exception]}]
  {:type :ThrowStatement
   :argument (generate exception)})

;; fn

(defn generate-fn-method [{:keys [body fixed-arity params variadic?]}]
  {:type :FunctionExpression
   :params (map identifier (take fixed-arity params))
   :body (block (when variadic?
                  (assign (identifier (last params))
                          {:type :CallExpression
                           :callee (identifier "Array.prototype.slice.call")
                           :arguments [(identifier "arguments")
                                       (literal fixed-arity)]}))
                (map generate body))})

(defn generate-fn-case [{:keys [fixed-arity variadic?] :as method}]
  {:type :SwitchCase :test (when-not variadic? (literal fixed-arity))
   :consequent [{:type :ReturnStatement
                 :argument {:type :CallExpression
                            :callee {:type :MemberExpression
                                     :object (generate-fn-method method)
                                     :property (identifier "apply") :computed false}
                            :arguments [(literal nil) (identifier "arguments")]}}]})

(defmethod generate-special :fn [{:keys [methods]}]
  (if (= (count methods) 1)
    (generate-fn-method (first methods))
    {:type :FunctionExpression :params []
     :body (block {:type :SwitchStatement
                   :discriminant (identifier "arguments.length")
                   :cases (map generate-fn-case methods)
                   :lexical false})}))

;; let, loop, recur

(defn generate-bindings
  ([bindings] (generate-bindings bindings #{}))
  ([bindings locals]
    (for [[k v] bindings]
      (assign (identifier (munge k))
              (if (contains? locals v) (identifier (munge v)) (generate v))))))

(defmethod generate-special :let [{:keys [bindings body]}]
  (block (generate-bindings bindings) (map generate body)))

(defmethod generate-special :loop [{:keys [bindings body env]}]
  (let [body* (block (generate-bindings bindings)
                     {:type :WhileStatement
                      :test (literal true)
                      :body (block (map generate body) {:type :BreakStatement})})]
    (if (= (:context env) :statement)
      body*
      {:type :CallExpression
       :callee {:type :FunctionExpression :body body* :params []}
       :arguments []})))

(defmethod generate-special :recur [{:keys [args recur-point]}]
  (let [rebinds (vec (map first (:bindings recur-point)))
        num-args (count args)
        temps (vec (take num-args (repeatedly gensym)))
        bindings (concat (map (juxt temps args) (range num-args))
                         (map (juxt rebinds temps) (range num-args)))]
    (block (generate-bindings bindings (set temps)) {:type :ContinueStatement})))

;; ns

(defn itself-or-empty-obj [ns-name-part]
  {:type :LogicalExpression :operator "||"
   :left (identifier (munge ns-name-part))
   :right {:type :ObjectExpression :properties []}})

(defn define-if-undefined [ns-name-part]
  (assign (identifier (munge ns-name-part))
          (itself-or-empty-obj ns-name-part)))

(defn ns-name-parts [ns-name]
  (let [subparts (str/split (str ns-name) #"\.")]
    (loop [parts [] idx 0]
      (if-let [subpart (get subparts idx)]
        (recur (conj parts (str (when (seq parts) (str (peek parts) ".")) subpart))
               (inc idx))
        parts))))

(defmethod generate-special :ns [{:keys [name]}]
  (let [[root & more] (ns-name-parts name)]
    (block {:type :VariableDeclaration :kind "var"
            :declarations [{:type :VariableDeclarator
                            :id (identifier (munge root))
                            :init (itself-or-empty-obj root)}]}
           (map define-if-undefined more))))

;; collections

(defmulti generate-collection :type)

(defmethod generate-collection :list [{:keys [children]}]
  (if (empty? children)
    (identifier "cljs.core.List.EMPTY")
    {:type :CallExpression
     :callee (identifier "cljs.core.list")
     :arguments (map generate children)}))

(defmethod generate-collection :vector [{:keys [children]}]
  (if (empty? children)
    (identifier "cljs.core.PersistentVector.EMPTY")
    {:type :CallExpression
     :callee (identifier "cljs.core.PersistentVector.fromArray")
     :arguments [(array (map generate children)) (literal true)]}))

(defmethod generate-collection :map [{:keys [children]}]
  (if (empty? children)
    (identifier "cljs.core.PersistentArrayMap.EMPTY")
    (let [pairs (map :children children)
          ks (map first pairs)
          vs (map second pairs)]
      {:type :NewExpression
       :callee (identifier "cljs.core.PersistentArrayMap.fromArray")
       :arguments [(array (map generate (interleave ks vs)))
                   (literal true) (literal false)]})))

(defmethod generate-collection :set [{:keys [children]}]
  (if (empty? children)
    (identifier "cljs.core.PersistentHashSet.EMPTY")
    {:type :CallExpression
     :callee (identifier "cljs.core.PersistentHashSet.fromArray")
     :arguments [(array (map generate children)) (literal true)]}))

;; constants

(defmulti generate-constant :type)

(defmethod generate-constant :keyword [{:keys [form]}]
  {:type :NewExpression
   :callee (identifier "cljs.core.Keyword")
   :arguments (map literal [nil (name form) (name form) (hash form)])})

(defmethod generate-constant :symbol [{:keys [form] {:keys [quoted?]} :env}]
  (let [name (name form)
        ns (namespace form)]
    (if quoted?
      {:type :NewExpression
       :callee (identifier "cljs.core.Symbol")
       :arguments (map literal
                    [ns name (str (when ns (str ns ".")) name) (hash form) nil])}
      (identifier
        (if (= ns "js")
          name
          (str (when ns (str (munge ns) ".")) (munge name)))))))

(defmethod generate-constant :default [{:keys [form]}]
  (literal form))

;; generic

(def block? #{:do :if :let :loop :recur})

(defn generate
  "Given an AST node `ast`, returns an equivalent JavaScript AST compatible
  with the SpiderMonkey Parser API (and, thus, Escodegen)."
  [{:keys [op] {:keys [context]} :env :as ast}]
  (let [js-ast (case op
                 :const (generate-constant ast)
                 :coll (generate-collection ast)
                 (generate-special ast))]
    (if (and (= context :return) (not (block? op)))
      {:type :ReturnStatement :argument js-ast}
      js-ast)))

(defn emit-all [asts]
  (.generate escodegen
    (-> (map generate asts) block (assoc :type :Program) clj->js)))

(defn emit [ast]
  (.generate escodegen (clj->js (generate ast))))
