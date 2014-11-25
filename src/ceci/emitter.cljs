(ns ceci.emitter
  (:require [clojure.string :as str]))

;; helpers

(declare ->js)

(def escodegen (js/require "escodegen"))

(defn nodes-seq [xs]
  (mapcat #(if ((some-fn sequential? nil?) %) % [%]) xs))

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

(defn array* [elements]
  {:type :ArrayExpression :elements elements})

(defn assign [left right]
  {:type :AssignmentExpression :operator "=" :left left :right right})

(defn literal [x]
  (if (and (number? x) (neg? x))
    ;; Escodegen chokes on negative number literals, because JavaScript
    {:type :UnaryExpression
     :operator "-" :prefix true
     :argument (literal (- x))}
    {:type :Literal :value x}))

(defn return [arg]
  {:type :ReturnStatement :argument arg})

(defn statement
  "Coerces expressions to statements. Leaves statements as they are."
  [expr-or-stmt]
  (if (re-find #"Statement|Declaration" (name (:type expr-or-stmt)))
    expr-or-stmt
    {:type :ExpressionStatement :expression expr-or-stmt}))

(defn block
  "Wraps `nodes`, zero or more AST nodes or sequences of AST nodes, in a single
  BlockStatement node, flattening nested BlockStatements."
  [& nodes]
  {:type :BlockStatement
   :body (->> (map statement (nodes-seq nodes))
              (mapcat #(if (= (:type %) :BlockStatement) (:body %) [%])))})

(defn ident [name]
  {:type :Identifier :name name})

(defn maybe-ident [x]
  (if (string? x) (ident x) x))

(defn call [f & args]
  {:type :CallExpression :callee (maybe-ident f) :arguments (nodes-seq args)})

(defn func [params & body]
  {:type :FunctionExpression
   :params (map ident params) :body (apply block body)})

(defn member [obj prop & [computed?]]
  {:type :MemberExpression
   :object obj :property (maybe-ident prop) :computed computed?})

(defn new* [ctor & args]
  (assoc (apply call ctor args) :type :NewExpression))

;; specials

(defmulti special->js :op)

(defmethod special->js :def [{:keys [name init]}]
  (assign (->js name) (->js init)))

(defmethod special->js :deftype [{:keys [name fields]}]
  (letfn [(assign-field [field]
            (let [fname (ident (munge field))]
              (assign (member {:type :ThisExpression} fname) fname)))]
    (assign (->js name) (func (map munge fields) (map assign-field fields)))))

(defmethod special->js :do [{:keys [body]}]
  (block (map ->js body)))

(defmethod special->js :if [{:keys [env test then else]}]
  {:type (if (= (:context env) :expr) :ConditionalExpression :IfStatement)
   :test (->js test)
   :consequent (statement (->js then))
   :alternate (statement (->js else))})

(defmethod special->js :invoke [{:keys [args], target :fn}]
  (call (member (->js target) "call") (literal nil) (map ->js args)))

(defmethod special->js :new [{:keys [ctor args]}]
  (new* (->js ctor) (map ->js args)))

(defmethod special->js :set! [{:keys [target val]}]
  (assign (->js target) (->js val)))

(defmethod special->js :throw [{:keys [env exception]}]
  {:type :ThrowStatement :argument (->js exception)})

;; fn

(defn method->js [{:keys [body fixed-arity params variadic?]}]
  (func (take fixed-arity params)
        (when variadic?
          (assign (ident (last params))
                  (call "Array.prototype.slice.call"
                        (ident "arguments") (literal fixed-arity))))
        (map ->js body)))

(defn method->js-case [{:keys [fixed-arity variadic?] :as method}]
  {:type :SwitchCase :test (when-not variadic? (literal fixed-arity))
   :consequent [(return (call (member (method->js method) "apply")
                              (literal nil) (ident "arguments")))]})

(defmethod special->js :fn [{:keys [methods]}]
  (if (= (count methods) 1)
    (method->js (first methods))
    (func [] {:type :SwitchStatement :lexical false
              :discriminant (ident "arguments.length")
              :cases (map method->js-case methods)})))

;; let, letfn, loop, recur

(defn bindings->js
  ([bindings] (bindings->js bindings #{}))
  ([bindings local?]
    {:type :VariableDeclaration :kind :var
     :declarations (for [[k v] bindings]
                     {:type :VariableDeclarator
                      :id (ident (munge k))
                      :init (if (local? v) (ident (munge v)) (->js v))})}))

(defmethod special->js :let [{:keys [bindings body]}]
  (block (bindings->js bindings) (map ->js body)))

(defmethod special->js :letfn [{:keys [bindings body]}]
  (block (bindings->js bindings) (map ->js body)))

(defmethod special->js :loop [{:keys [bindings body env]}]
  (let [body (block (bindings->js bindings)
                    {:type :WhileStatement :test (literal true)
                     :body (block (map ->js body) {:type :BreakStatement})})]
    (if (= (:context env) :statement) body (call (func [] body)))))

(defmethod special->js :recur [{:keys [args recur-point]}]
  (let [rebinds (vec (map first (:bindings recur-point)))
        num-args (count args)
        temps (vec (take num-args (repeatedly gensym)))
        bindings (concat (map (juxt temps args) (range num-args))
                         (map (juxt rebinds temps) (range num-args)))]
    (block (bindings->js bindings (set temps)) {:type :ContinueStatement})))

;; ns

(defn itself-or-empty-obj [ident]
  {:type :LogicalExpression :operator "||"
   :left ident :right {:type :ObjectExpression :properties []}})

(defn define-if-undefined [ident]
  (assign ident (itself-or-empty-obj ident)))

(defn ancestor-ns-names [ns-name]
  (let [parts (str/split ns-name #"\.")]
    (for [n (map inc (range (count parts)))]
      (str/join \. (take n parts)))))

(defmethod special->js :ns [{:keys [name]}]
  (let [[root & more] (map (comp ident munge) (ancestor-ns-names name))]
    (block {:type :VariableDeclaration :kind :var
            :declarations [{:type :VariableDeclarator
                            :id root :init (itself-or-empty-obj root)}]}
           (map define-if-undefined more))))

;; collections

(defmulti coll->js :type)

(defmethod coll->js :list [{:keys [children]}]
  (if (empty? children)
    (ident "cljs.core.List.EMPTY")
    (call "cljs.core.list" (map ->js children))))

(defmethod coll->js :vector [{:keys [children]}]
  (if (empty? children)
    (ident "cljs.core.PersistentVector.EMPTY")
    (call "cljs.core.PersistentVector.fromArray"
          (array* (map ->js children)) (literal true))))

(defmethod coll->js :map [{:keys [children]}]
  (if (empty? children)
    (ident "cljs.core.PersistentArrayMap.EMPTY")
    (let [pairs (map :children children)
          ks (map first pairs)
          vs (map second pairs)]
      (new* "cljs.core.PersistentArrayMap.fromArray"
            (array* (map ->js (interleave ks vs)))
            (literal true) (literal false)))))

(defmethod coll->js :set [{:keys [children]}]
  (if (empty? children)
    (ident "cljs.core.PersistentHashSet.EMPTY")
    (call "cljs.core.PersistentHashSet.fromArray"
          (array* (map ->js children)) (literal true))))

;; constants

(defmulti const->js :type)

(defmethod const->js :keyword [{:keys [form]}]
  (new* "cljs.core.Keyword"
        (map literal [nil (name form) (name form) (hash form)])))

(defmethod const->js :symbol [{:keys [form] {:keys [quoted?]} :env}]
  (let [name (name form)
        ns (namespace form)]
    (if quoted?
      (new* "cljs.core.Symbol"
            (map literal [ns name (str form) (hash form) nil]))
      (ident (if (= ns "js")
               name
               (str (when ns (str (munge ns) ".")) (munge name)))))))

(defmethod const->js :default [{:keys [form]}]
  (literal form))

;; generic

(def block? #{:do :if :let :letfn :loop :recur})

(defn ->js
  "Converts a ClojureScript AST node to a JavaScript AST node."
  [{:keys [op] {:keys [context]} :env :as ast}]
  (cond-> (case op :const (const->js ast)
                   :coll (coll->js ast)
                   (special->js ast))
          (and (= context :return) (not (block? op))) return))

(defn emit-all [asts]
  (.generate escodegen
    (-> (map ->js asts) block (assoc :type :Program) clj->js)))

(defn emit [ast]
  (.generate escodegen (clj->js (->js ast))))
