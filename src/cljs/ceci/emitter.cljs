(ns ceci.emitter
  (:require [ceci.util :refer [in?]]
            [clojure.string :as string]))

(declare emit)

;; helper fns

(defn emit-escaped [s]
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

(defn emit-statements [statements]
  (string/join (map emit statements)))

(defn wrap-fn [& exprs]
  (str "(function(){" (string/join exprs) "})()"))

(defn comma-sep [args]
  (->> args (map emit) (string/join ",")))

(defn wrap-quotes [s]
  (str "\"" s "\""))

;; special forms

(defmulti emit-special :op)

(defn emit-aget [{:keys [target fields]}]
  (letfn [(emit-field [field] (str "[" (emit field) "]"))]
    (apply str (emit target) (map emit-field fields))))

(defmethod emit-special :aget [ast]
  (emit-aget ast))

(defmethod emit-special :aset [{:keys [value] :as ast}]
  (str (emit-aget ast) "=" (emit value)))

(defmethod emit-special :def [{:keys [name init]}]
  (str (emit name) "=" (emit init)))

(defmethod emit-special :do [{:keys [env body]}]
  (if (= (:context env) :expr)
      (wrap-fn (emit-statements body))
      (emit-statements body)))

(defmethod emit-special :if [{:keys [env test then else]}]
  (if (= (:context env) :expr)
      (str "((" (emit test) ")?" (emit then) ":" (emit else) ")")
      (str "if(" (emit test) "){" (emit then)
           "}else{" (emit else) "}")))

(defmethod emit-special :invoke [{:keys [invoked args]}]
  (str (emit invoked) ".call(null" (when (seq args) ",") (comma-sep args) ")"))

(defmethod emit-special :new [{:keys [ctor args]}]
  (str "(new " (emit ctor) "(" (comma-sep args) "))"))

(defmethod emit-special :throw [{:keys [thrown]}]
  (wrap-fn "throw " (emit thrown) ";\n"))

;; function forms

(defn emit-params [params]
  (when-not (empty? params)
    (letfn [(emit-param [idx]
              (let [param (get params idx)]
                (str (emit-escaped param) "="
                     (if (:rest-param? (meta param))
                         (str "Array.prototype.slice.call(arguments," idx ")")
                         (str "arguments[" idx "]")))))]
      (str (->> (range (count params))
                (map emit-param)
                (string/join ";\n")) ";\n"))))

(defn emit-fn-clause [[num-params {:keys [params body]}]]
  (str "case " num-params ":" (emit-params params) (emit-statements body)))

(defmethod emit-special :fn [{:keys [clauses]}]
  (if (= (count clauses) 1)
      (let [{:keys [params body]} (val (first clauses))]
        (str "(function(){" (emit-params params)
             (emit-statements body) "})"))
      (str "(function(){switch(arguments.length){"
           (string/join (map emit-fn-clause clauses))
           "default:throw new Error("
           "\"invalid function arity (\" + arguments.length + \")\""
           ");}})")))

;; let, loop and recur forms

(defn emit-bindings
  ([bindings] (emit-bindings bindings []))
  ([bindings temps]
    (when (> (count bindings) 0)
      (str (->> bindings
                (map (fn [[k v]]
                       (str (emit-escaped k) "="
                            (if (in? temps v) (emit-escaped v) (emit v)))))
                (string/join ";\n")) ";\n"))))

(defmethod emit-special :let [{:keys [env bindings body]}]
  (if (= (:context env) :expr)
      (wrap-fn (emit-bindings bindings) (emit-statements body))
      (str (emit-bindings bindings) (emit-statements body))))

(defmethod emit-special :loop [{:keys [env bindings body]}]
  (if (= (:context env) :expr)
      (wrap-fn (emit-bindings bindings)
                    "while(true){"
                    (emit-statements body)
                    "break;\n}")
      (str (emit-bindings bindings) "while(true){"
           (emit-statements body) "break;\n}")))

(defmethod emit-special :recur [{:keys [args recur-point]}]
  (let [rebinds (vec (map first (:bindings recur-point)))
        num-args (count args)
        temps (vec (take num-args (repeatedly gensym)))
        bindings (concat (map (juxt temps args) (range num-args))
                         (map (juxt rebinds temps) (range num-args)))]
    (str (emit-bindings bindings temps) "continue;\n")))

;; collection forms

(defmulti emit-collection :type)

(defmethod emit-collection :list [{:keys [children]}]
  (if (empty? children)
      "cljs.core.List.EMPTY"
      (str "cljs.core.list(" (comma-sep children) ")")))

(defmethod emit-collection :vector [{:keys [children]}]
  (if (empty? children)
      "cljs.core.PersistentVector.EMPTY"
      (str "cljs.core.PersistentVector.fromArray(["
           (comma-sep children) "],true)")))

(defmethod emit-collection :map [{:keys [children]}]
  (if (empty? children)
      "cljs.core.PersistentArrayMap.EMPTY"
      (let [pairs (map :children children)
            ks (map first pairs)
            vs (map second pairs)]
        (str "new cljs.core.PersistentArrayMap.fromArray(["
             (comma-sep (interleave ks vs)) "],true,false)"))))

(defmethod emit-collection :set [{:keys [children]}]
  (if (empty? children)
      "cljs.core.PersistentHashSet.EMPTY"
      (str "cljs.core.PersistentHashSet.fromArray(["
           (comma-sep children) "],true)")))

;; constant forms

(defmulti emit-constant :type)

(defmethod emit-constant :bool [{:keys [form]}]
  (str form))

(defmethod emit-constant :keyword [{:keys [form]}]
  (let [name (name form)]
    (str "new cljs.core.Keyword(null,"
         (wrap-quotes name) "," (wrap-quotes name) "," (hash form) ")")))

(defmethod emit-constant :nil [_]
  "null")

(defmethod emit-constant :number [{:keys [form]}]
  (str form))

(defmethod emit-constant :string [{:keys [form]}]
  (wrap-quotes form))

(defmethod emit-constant :symbol [{:keys [form] {:keys [quoted?]} :env}]
  (let [name (name form)
        ns (namespace form)]
    (if quoted?
        (str "new cljs.core.Symbol("
             (if ns (wrap-quotes ns) "null") ","
             (wrap-quotes name) ","
             (wrap-quotes (str (when ns (str ns ".")) name)) ","
             (hash form) ",null)")
        (str (when (and ns (not= ns "js")) (str (emit-escaped ns) "."))
             (emit-escaped name)))))

;; generic interface

(def block? #{:if :let :loop :recur})

(defn emit
  "Given an AST node `ast`, returns a string of equivalent JavaScript code."
  [{:keys [op] {:keys [context]} :env :as ast}]
  (str (when (and (= context :return) (not (block? op))) "return ")
       (condp = op
         :const (emit-constant ast)
         :coll (emit-collection ast)
         (emit-special ast))
       (when-not (or (= context :expr) (block? op)) ";\n")))
 