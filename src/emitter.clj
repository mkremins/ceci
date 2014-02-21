(ns emitter
  (:require [clojure.string :as string]))

(declare emit-def emit-if emit-let emit-fn emit-do)

(def special-form
  {"def" emit-def
   "if"  emit-if
   "let" emit-let
   "fn"  emit-fn
   "do"  emit-do})

(declare emit)

;; list forms

(defn emit-fncall [callable args]
  (str (emit callable) ".call(null," (string/join "," (map emit args)) ")"))

(defn emit-list [{:keys [children]}]
  (if-let [{:keys [type value] :as first-child} (first children)]
    (if (and (= type :symbol) (special-form value))
      (let [emit-special-form (special-form value)]
        (emit-special-form type (rest children)))
      (emit-fncall first-child (rest children)))
    "new List()"))

;; other forms

(declare emit-vector emit-map)

(defn emit-number [{:keys [value]}]
  (str value))

(defn emit-keyword [{:keys [value]}]
  (str "new Keyword(\"" (string/join (drop 1 value)) "\")"))

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

;; generic interface

(defn emit [{:keys [type value] :as ast-node}]
  (condp = type
    :list (emit-list ast-node)
    :vector (emit-vector ast-node)
    :map (emit-map ast-node)
    :number (emit-number ast-node)
    :keyword (emit-keyword ast-node)
    :string (emit-string ast-node)
    :symbol (if-let [emit-special-form (special-form value)]
              (emit-special-form ast-node)
              (emit-symbol ast-node))))
