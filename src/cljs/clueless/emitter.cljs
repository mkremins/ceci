(ns clueless.emitter
  (:require [clojure.string :as string]
            [clueless.env :refer [symbol-parts]]))

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

(defn emit-args [args]
  (->> args (map emit) (string/join ",")))

;; special forms

(defn emit-def [{:keys [name init]}]
  (str (emit name) "=" (emit init)))

(defn emit-do [{:keys [body]}]
  (emit-wrapped (emit-expr-block body)))

(defn emit-let [{:keys [bindings body]}]
  (letfn [(emit-binding [[k v]]
            (str "var " (emit-escaped k) "=" (emit v)))]
    (emit-wrapped (string/join ";" (map emit-binding bindings)) ";"
                  (emit-expr-block body))))

(defn emit-if [{:keys [test then else]}]
  (emit-wrapped "if(" (emit test) "){" (emit-return then)
                "}else{" (emit-return else) "}"))

;; function forms

(defn emit-params [params]
  (->> (range (count params))
    (map (fn [param-num]
           (str "var " (emit-escaped (get params param-num))
                "=arguments[" param-num "]")))
    (string/join ";")))

(defn emit-fn-clause [[num-params {:keys [params] :as clause}]]
  (str "case " num-params ":" (emit-params params)
       ";return " (emit-do clause)))

(defn emit-fn [{:keys [name clauses]}]
  (let [name (emit-escaped (:form name))]
    (if (= (count clauses) 1)
      (let [{:keys [params body]} (val (first clauses))]
        (str "var " name "=function(){"
             "var recur=" name ";"
             (emit-params params) ";"
             (emit-expr-block body) "}"))
      (str "var " name "=function(){"
           "var recur=" name ";"
           "switch(arguments.length){"
           (string/join ";" (map emit-fn-clause clauses))
           ";default:throw new Error("
           "\"invalid function arity (\" + arguments.length + \")\""
           ");}}"))))

;; list forms

(defn emit-fncall [callable args]
  (str (emit callable) ".call(null," (string/join "," (map emit args)) ")"))

(defn emit-list [{:keys [children] {:keys [quoted?]} :env}]
  (if-let [{:keys [type value] :as first-child} (first children)]
    (if quoted?
        (str "new List(" (emit-args children) ")")
        (emit-fncall first-child (rest children)))
    "List.EMPTY"))

;; other forms

(defn emit-vector [{:keys [children]}]
  (if (> (count children) 0)
      (str "new Vector(" (emit-args children) ")")
      "Vector.EMPTY"))

(defn emit-map [{:keys [children]}]
  (if (> (count children) 0)
      (str "new Map(" (emit-args children) ")")
      "Map.EMPTY"))

(defn emit-set [{:keys [children]}]
  (if (> (count children) 0)
      (str "new Set(" (emit-args children) ")")
      "Set.EMPTY"))

(defn emit-number [{:keys [form]}]
  (str form))

(defn emit-keyword [{:keys [form]}]
  (str "new Keyword(\"" (name form) "\")"))

(defn emit-string [{:keys [form]}]
  (str "\"" form "\""))

(defn emit-symbol [{:keys [form] {:keys [quoted?]} :env}]
  (if quoted?
      (str "new Symbol(\"" (name form) "\")")
      (let [[ns-part? name-part] (symbol-parts form)
            ns-part (when ns-part? (str (emit-escaped ns-part?) "."))
            name-part (emit-escaped name-part)]
        (str ns-part name-part))))

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
   :def emit-def
   :do emit-do
   :fn emit-fn
   :if emit-if
   :let emit-let})

(defn emit [{:keys [op type] :as ast-node}]
  (if (#{:const :coll} op)
      (let [emit-type (emitters type)]
        (emit-type ast-node))
      (let [emit-op (emitters op)]
        (emit-op ast-node))))
