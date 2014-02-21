(ns clueless.emitter
  (:require [clojure.string :as string]))

(declare emit-def emit-if emit-let emit-fn)

(declare emit)

(defn emit-do [{:keys [body]}]
  (let [exprs (drop-last body)
        return-expr (last body)]
    (str "(function(){"
         (string/join ";" (map emit exprs))
         ";return " (emit return-expr) ";"
         "})()")))

;; list forms

(defn emit-fncall [callable args]
  (str (emit callable) ".call(null," (string/join "," (map emit args)) ")"))

(defn emit-list [{:keys [children]}]
  (if-let [{:keys [type value] :as first-child} (first children)]
    (emit-fncall first-child (rest children))
    "new List()"))

;; other forms

(declare emit-vector emit-map)

(defn emit-number [{:keys [value]}]
  (str value))

(defn emit-keyword [{:keys [value]}]
  (str "new Keyword(\"" value "\")"))

(defn emit-string [{:keys [value]}]
  (str "\"" value "\""))

(defn emit-symbol [{:keys [value]}]
  (-> value
    (string/replace #"\+" "_PLUS_")
    (string/replace #"-" "_")
    (string/replace #"\*" "_STAR_")
    (string/replace #"/" "_SLASH_")
    (string/replace #"\?" "_QMARK_")
    (string/replace #"!" "_BANG_")
    (string/replace #"<" "_LT_")
    (string/replace #">" "_GT_")
    (string/replace #"=" "_EQ_")))

(defn emit-bool [{:keys [value]}]
  (str value))

(defn emit-nil [_]
  "null")

;; generic interface

(defn emit [{:keys [type value] :as ast-node}]
  (condp = type
    :list (emit-list ast-node)
    :vector (emit-vector ast-node)
    :map (emit-map ast-node)
    :number (emit-number ast-node)
    :keyword (emit-keyword ast-node)
    :string (emit-string ast-node)
    :symbol (emit-symbol ast-node)
    :bool (emit-bool ast-node)
    :nil (emit-nil ast-node)
    :def (emit-def ast-node)
    :do (emit-do ast-node)
    :let (emit-let ast-node)
    :fn (emit-fn ast-node)
    :if (emit-if ast-node)))
