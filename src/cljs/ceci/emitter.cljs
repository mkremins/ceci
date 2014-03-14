(ns ceci.emitter
  (:require [clojure.string :as string]))

(declare emit)

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

(defn emit-return [expr]
  (str "return " (emit expr) ";"))

(defn emit-expr-block [exprs]
  (let [body (drop-last exprs)
        retval (last exprs)]
    (str (string/join ";" (map emit body))
         (when (> (count body) 0) ";") (emit-return retval))))

(defn emit-wrapped [& exprs]
  (str "(function(){" (string/join exprs) "})()"))

(defn comma-sep [args]
  (->> args (map emit) (string/join ",")))

;; special forms

(defn emit-aget [{:keys [target fields]}]
  (letfn [(emit-field [field] (str "[" (emit field) "]"))]
    (apply str (emit target) (map emit-field fields))))

(defn emit-aset [{:keys [value] :as ast}]
  (str (emit-aget ast) "=" (emit value)))

(defn emit-def [{:keys [name init]}]
  (str (emit name) "=" (emit init)))

(defn emit-do [{:keys [body]}]
  (emit-wrapped (emit-expr-block body)))

(defn emit-let [{:keys [bindings body]}]
  (letfn [(emit-binding [[k v]]
            (str (emit-escaped k) "=" (emit v)))]
    (emit-wrapped (string/join ";" (map emit-binding bindings)) ";"
                  (emit-expr-block body))))

(defn emit-if [{:keys [test then else]}]
  (emit-wrapped "if(" (emit test) "){" (emit-return then)
                "}else{" (emit-return else) "}"))

(defn emit-new [{:keys [ctor args]}]
  (str "(new " (emit ctor) "(" (comma-sep args) "))"))

(defn emit-throw [{:keys [thrown]}]
  (emit-wrapped "throw " (emit thrown) ";"))

;; function forms

(defn emit-params [params]
  (when-not (empty? params)
    (str (->> (range (count params))
              (map (fn [param-num]
                     (str (emit-escaped (get params param-num))
                          "=arguments[" param-num "]")))
              (string/join ";")) ";")))

(defn emit-fn-clause [[num-params {:keys [params] :as clause}]]
  (str "case " num-params ":" (emit-params params)
       "return " (emit-do clause)))

(defn emit-fn [{:keys [clauses]}]
  (if (= (count clauses) 1)
      (let [{:keys [params body]} (val (first clauses))]
        (str "(function(){" (emit-params params) (emit-expr-block body) "})"))
      (str "(function(){switch(arguments.length){"
           (string/join ";" (map emit-fn-clause clauses))
           ";default:throw new Error("
           "\"invalid function arity (\" + arguments.length + \")\""
           ");}})")))

;; list forms

(defn emit-fncall [callable args]
  (str (emit callable) ".call(null," (string/join "," (map emit args)) ")"))

(defn emit-list [{:keys [children] {:keys [quoted?]} :env}]
  (if-let [{:keys [type value] :as first-child} (first children)]
    (if quoted?
        (str "cljs.core.list(" (comma-sep children) ")")
        (emit-fncall first-child (rest children)))
    "cljs.core.List.EMPTY"))

;; other forms

(defn emit-vector [{:keys [children]}]
  (if (empty? children)
      "cljs.core.PersistentVector.EMPTY"
      (str "cljs.core.PersistentVector.fromArray(["
           (comma-sep children) "],true)")))

(defn emit-map [{:keys [children]}]
  (if (empty? children)
      "cljs.core.PersistentArrayMap.EMPTY"
      (str "new cljs.core.PersistentArrayMap.fromArray(["
           (comma-sep children) "],true,false)")))

(defn emit-set [{:keys [children]}]
  (if (empty? children)
      "cljs.core.PersistentHashSet.EMPTY"
      (str "cljs.core.PersistentHashSet.fromArray(["
           (comma-sep children) "],true)")))

(defn emit-number [{:keys [form]}]
  (str form))

(defn emit-keyword [{:keys [form]}]
  (let [name (name form)]
    (str "new cljs.core.Keyword(null,\""
         name "\",\"" name "\"," (hash form) ")")))

(defn emit-string [{:keys [form]}]
  (str "\"" form "\""))

(defn emit-symbol [{:keys [form] {:keys [quoted?]} :env}]
  (let [ns (namespace form)
        name (name form)]
    (if quoted?
        (str "new cljs.core.Symbol("
             (if ns (str "\"" ns "\"") "null") ",\"" name "\",\""
             (str (when ns (str ns ".")) name) "\"," (hash form) ",null)")
        (str (when (and ns (not= ns "js")) (str (emit-escaped ns) "."))
             (emit-escaped name)))))

(defn emit-bool [{:keys [form]}]
  (str form))

(defn emit-nil [_]
  "null")

;; generic interface

(def emitters
  {:list emit-list
   :vector emit-vector
   :map emit-map
   :set emit-set
   :number emit-number
   :keyword emit-keyword
   :string emit-string
   :symbol emit-symbol
   :bool emit-bool
   :nil emit-nil
   :aget emit-aget
   :aset emit-aset
   :def emit-def
   :do emit-do
   :fn emit-fn
   :if emit-if
   :let emit-let
   :new emit-new
   :throw emit-throw})

(defn emit [{:keys [op type] :as ast-node}]
  (if (#{:const :coll} op)
      (let [emit-type (emitters type)]
        (emit-type ast-node))
      (let [emit-op (emitters op)]
        (emit-op ast-node))))
