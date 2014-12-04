(ns ceci.emitter
  (:require [clojure.string :as str]))

;; helpers

(declare ->js)

(def escodegen (js/require "escodegen"))

(defn nodes-seq [xs]
  (mapcat #(if ((some-fn sequential? nil?) %) % [%]) xs))

(defn munge [s]
  (-> (str s)
      (str/replace \+ "_PLUS_")
      (str/replace \- "_")
      (str/replace \* "_STAR_")
      (str/replace \/ "_SLASH_")
      (str/replace \? "_QMARK_")
      (str/replace \! "_BANG_")
      (str/replace \< "_LT_")
      (str/replace \> "_GT_")
      (str/replace \= "_EQ_")))

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
   :params params :body (apply block body)})

(defn member [obj prop & [computed?]]
  {:type :MemberExpression
   :object obj :property (maybe-ident prop) :computed computed?})

(defn new* [ctor & args]
  (assoc (apply call ctor args) :type :NewExpression))

;; specials

(defmulti special->js :op)

(defmethod special->js :binding [{:keys [name init]}]
  (assign (ident (munge name)) (->js init)))

(defmethod special->js :def [{:keys [var init]}]
  (assign (->js var) (->js init)))

(defmethod special->js :deftype [{:keys [var fields]}]
  (let [fnames (map (comp ident munge) fields)
        assign-field #(assign (member {:type :ThisExpression} %) %)]
    (assign (->js var) (func fnames (map assign-field fnames)))))

(defmethod special->js :do [{:keys [body]}]
  (block (map ->js body)))

(defn method->js [{:keys [body fixed-arity params variadic?]}]
  (func (map ->js (take fixed-arity params))
        (when variadic?
          (assign (->js (last params))
                  (call "cljs.core.array_seq"
                        (call "Array.prototype.slice.call"
                              (ident "arguments") (literal fixed-arity)))))
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

(defmethod special->js :if [{:keys [env test then else]}]
  {:type (if (= (:context env) :expr) :ConditionalExpression :IfStatement)
   :test (->js test)
   :consequent (statement (->js then))
   :alternate (statement (->js else))})

(defmethod special->js :invoke [{:keys [args], target :fn}]
  (call (member (->js target) "call") (literal nil) (map ->js args)))

(defmethod special->js :js-var [{:keys [form]}]
  (ident (name form)))

(defmethod special->js :let [{:keys [bindings body]}]
  (block (map ->js bindings) (map ->js body)))

(defmethod special->js :letfn [{:keys [bindings body]}]
  (block (map ->js bindings) (map ->js body)))

(defmethod special->js :local [{:keys [name]}]
  (ident (munge name)))

(defmethod special->js :loop [{:keys [bindings body env]}]
  (let [body (block (map ->js bindings)
                    {:type :WhileStatement :test (literal true)
                     :body (block (map ->js body) {:type :BreakStatement})})]
    (if (= (:context env) :statement) body (call (func [] body)))))

(defmethod special->js :new [{:keys [ctor args]}]
  (new* (->js ctor) (map ->js args)))

(defn itself-or-empty-obj [ident]
  {:type :LogicalExpression :operator "||"
   :left ident :right {:type :ObjectExpression :properties []}})

(defn define-if-undefined [ident]
  (assign ident (itself-or-empty-obj ident)))

(defn ancestor-ns-names [ns-name]
  (let [parts (str/split (str ns-name) #"\.")]
    (for [n (map inc (range (count parts)))]
      (str/join \. (take n parts)))))

(defmethod special->js :ns [{:keys [name]}]
  (let [[root & more] (map (comp ident munge) (ancestor-ns-names name))]
    (block {:type :VariableDeclaration :kind :var
            :declarations [{:type :VariableDeclarator
                            :id root :init (itself-or-empty-obj root)}]}
           (map define-if-undefined more))))

(defmethod special->js :recur [{:keys [args recur-point]}]
  (block (map #(->js (assoc %1 :init %2)) (:bindings recur-point) args)
         {:type :ContinueStatement}))

(defmethod special->js :set! [{:keys [target val]}]
  (assign (->js target) (->js val)))

(defmethod special->js :throw [{:keys [env exception]}]
  {:type :ThrowStatement :argument (->js exception)})

(defmethod special->js :var [{:keys [ns name]}]
  (ident (str (munge ns) "." (munge name))))

;; collections

(defmulti coll->js :type)

(defmethod coll->js :list [{:keys [items]}]
  (if (empty? items)
    (ident "cljs.core.List.EMPTY")
    (call "cljs.core.list" (map ->js items))))

(defmethod coll->js :vector [{:keys [items]}]
  (if (empty? items)
    (ident "cljs.core.PersistentVector.EMPTY")
    (call "cljs.core.PersistentVector.fromArray"
          (array* (map ->js items)) (literal true))))

(defmethod coll->js :map [{:keys [keys vals]}]
  (if (empty? keys)
    (ident "cljs.core.PersistentArrayMap.EMPTY")
    (new* "cljs.core.PersistentArrayMap.fromArray"
          (array* (map ->js (interleave keys vals)))
          (literal true) (literal false))))

(defmethod coll->js :set [{:keys [items]}]
  (if (empty? items)
    (ident "cljs.core.PersistentHashSet.EMPTY")
    (call "cljs.core.PersistentHashSet.fromArray"
          (array* (map ->js items)) (literal true))))

;; constants

(defmulti const->js :type)

(defmethod const->js :keyword [{:keys [form]}]
  (new* "cljs.core.Keyword"
        (map literal [nil (name form) (name form) (hash form)])))

(defmethod const->js :symbol [{:keys [form]}]
  (new* "cljs.core.Symbol"
        (map literal [(namespace form) (name form) (str form) (hash form) nil])))

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
