(ns ceci.expander
  (:require [ceci.util :refer [merge-meta raise]]))

(defn expand-macro [form macros]
  (if-let [macro (macros (first form))]
    (let [metadata (meta form)]
      (merge-meta (apply macro (rest form)) metadata))
    form))

(defn desugar-new-syntax
  "Desugars (Ctor. args) to (new Ctor args)."
  [[ctor & args :as form]]
  (let [cname (name ctor)]
    (if (= (last cname) \.)
      (let [cname (apply str (drop-last cname))
            cns (namespace ctor)]
        (list* 'new (if cns (symbol cns cname) (symbol cname)) args))
      form)))

;; public API

(defn expand-once [form macros]
  (if (and (list? form) (symbol? (first form)))
    (-> form (expand-macro macros) desugar-new-syntax)
    form))

(defn expand
  "Expands `form` repeatedly using `expand-once` until the result cannot be
  expanded further, then returns the result."
  [form macros]
  (loop [original form
         expanded (expand-once form macros)]
    (if (= original expanded)
      original
      (recur expanded (expand-once expanded macros)))))

(defn expand-all
  [form macros]
  "Walks `form` from the top down, applying `expand` to each subform, and
  returns the result."
  (let [form (expand form macros)
        metadata (meta form)
        expand-all* #(expand-all % macros)
        expanded (condp #(%1 %2) form
                   map? (zipmap (map expand-all* (keys form))
                                (map expand-all* (vals form)))
                   (some-fn list? seq?) (map expand-all* form)
                   coll? (into (empty form) (map expand-all* form))
                   form)]
    (merge-meta expanded metadata)))

;; syntax-quote

(declare syntax-quote)

(defn unquote? [form]
  (and (list? form) (= (first form) 'unquote)))

(defn unquote-splice? [form]
  (and (list? form) (= (first form) 'unquote-splice)))

(defn expand-sequence
  "Propagates `syntax-quote` over `forms`, expanding any `unquote-splice`s."
  [forms]
  (cons 'concat (map #(if (unquote-splice? %)
                        (list 'seq (second %)) [(syntax-quote %)])
                     forms)))

(defn syntax-quote [form]
  (condp #(%1 %2) form
    symbol? (list 'quote form)
    unquote? (second form)
    unquote-splice? (raise "invalid location for ~@" form)
    (some-fn (complement coll?) empty?) form
    list? (expand-sequence form)
    map? (list 'apply 'hash-map (expand-sequence (apply concat form)))
    set? (list 'set (expand-sequence form))
    vector? (list 'vec (expand-sequence form))
    (raise "unknown collection type" form)))
