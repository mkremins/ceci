(ns clueless.emitter
  (:require [clojure.string :as string]))

(declare emit)

(defn emit-escaped [s]
  (-> s
    (string/replace #"\+" "_PLUS_")
    (string/replace #"-" "_")
    (string/replace #"\*" "_STAR_")
    (string/replace #"/" "_SLASH_")
    (string/replace #"\?" "_QMARK_")
    (string/replace #"!" "_BANG_")
    (string/replace #"<" "_LT_")
    (string/replace #">" "_GT_")
    (string/replace #"=" "_EQ_")))

(defn emit-expr-block [exprs]
  (let [body (drop-last exprs)
        return (last exprs)]
    (str (string/join ";" (map emit body))
         (when (> (count body) 0) ";") "return " (emit return) ";")))

;; special forms

(defn emit-def [{:keys [name value]}]
  (str "window." (emit-escaped name) "=" (emit value) ";"))

(defn emit-do [{:keys [body]}]
  (str "(function(){" (emit-expr-block body) "})()"))

(defn emit-let [{:keys [bindings body]}]
  (letfn [(emit-binding [[k v]]
            (str "var " (emit-escaped k) "=" (emit v)))]
    (str "(function(){"
         (string/join ";" (map emit-binding bindings)) ";"
         (emit-expr-block body) "})()")))

(defn emit-if [{:keys [expr then else]}]
  (str "(function(){if(" (emit expr)
       "){return " (emit then)
       ";}else{return " (emit else) ";}})()"))

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
  (if (= (count clauses) 1)
    (let [{:keys [params body]} (val (first clauses))]
      (str "function " (emit-escaped name) "(){"
           (emit-params params) ";"
           (emit-expr-block body) "}"))
    (str "function " (emit-escaped name) "(){"
         "switch(arguments.length){"
         (string/join ";" (map emit-fn-clause clauses))
         ";default:throw new Error("
         "\"invalid function arity (\" + arguments.length + \")\""
         ");}}")))

;; list forms

(defn emit-fncall [callable args]
  (str (emit callable) ".call(null," (string/join "," (map emit args)) ")"))

(defn emit-list [{:keys [children]}]
  (if-let [{:keys [type value] :as first-child} (first children)]
    (emit-fncall first-child (rest children))
    "new List()"))

;; other forms

(defn emit-vector [{:keys [children]}]
  (if (> (count children) 0)
    (str "new Vector(" (->> children (map emit) (string/join ",")) ")")
    "new Vector()"))

(defn emit-map [{:keys [pairs]}]
  (letfn [(emit-pair [[k v]]
            (str "[" (emit k) "," (emit v) "]"))]
    (if (> (count pairs) 0)
      (str "new Map(" (->> pairs (map emit-pair) (string/join ",")) ")")
      "new Map()")))

(defn emit-number [{:keys [value]}]
  (str value))

(defn emit-keyword [{:keys [value]}]
  (str "new Keyword(\"" value "\")"))

(defn emit-string [{:keys [value]}]
  (str "\"" value "\""))

(defn emit-symbol [{:keys [value]}]
  (emit-escaped value))

(defn emit-bool [{:keys [value]}]
  (str value))

(defn emit-nil [_]
  "null")

;; generic interface

(def emitters
  {:list emit-list
   :vector emit-vector
   :map emit-map
   :number emit-number
   :keyword emit-keyword
   :string emit-string
   :symbol emit-symbol
   :bool emit-bool
   :nil emit-nil
   :def emit-def
   :do emit-do
   :let emit-let
   :fn emit-fn
   :if emit-if})

(defn emit [{:keys [type] :as ast-node}]
  (let [emit-type (emitters type)]
    (emit-type ast-node)))
