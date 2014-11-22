(ns ceci.expander
  (:require [ceci.util :refer [merge-meta raise]]))

;; macros

(defn expand-macro
  "Assumes `form` is a list form. If `(first form)` names a macro, retrieves
  the named macro and uses it to expand `form`, returning the result.
  Otherwise, returns `form`."
  [form macros]
  (if-let [expander (macros (first form))]
    (let [metadata (meta form)]
      (merge-meta (apply expander (rest form)) metadata))
    form))

;; syntax sugar

(defn desugar-new-syntax
  "Assumes `form` is a list form. If `(first form)` ends with a period,
  desugars `form` to the canonical constructor-invoke syntax `(new ctor args)`
  and returns the result. Otherwise, returns `form`."
  [[ctor & args :as form]]
  (let [name (name ctor)
        ns   (namespace ctor)]
    (if (= (last name) ".")
      (let [name* (subs name 0 (dec (count name)))]
        (apply list 'new (if ns (symbol ns name*) (symbol name*)) args))
      form)))

(defn desugar-field-access
  "Assumes `form` is a list form. If `(first form)` starts with a period
  followed by a dash, desugars `form` to the canonical field-access syntax
  `(aget obj fname)` and returns the result. Otherwise, returns `form`."
  [form]
  (let [field (str (first form))]
    (if (and (= (first field) ".") (= (second field) "-"))
        (list 'aget (second form) (subs field 2))
        form)))

(defn desugar-method-call
  "Assumes `form` is a list form. If `(first form)` starts with a period,
  desugars `form` to the canonical method-call syntax `((aget obj mname) args)`
  and returns the result. Otherwise, returns `form`."
  [form]
  (let [method (str (first form))]
    (if (= (first method) ".")
        (cons (list 'aget (second form) (subs method 1))
              (drop 2 form))
        form)))

;; public API

(defn expand-once
  "Expands `form` once. If `form` is a list with a registered macro's name as
  its first element, applies the corresponding macro to the remaining elements
  of `form` and returns the result; otherwise, returns `form`."
  [form macros]
  (if (and (list? form) (symbol? (first form)))
    (-> form
        (expand-macro macros)
        desugar-new-syntax
        desugar-field-access
        desugar-method-call)
    form))

(defn expand
  "Expands `form` repeatedly (using `expand-once`) until the result no longer
  represents a macro form, then returns the result. Does not recursively expand
  any children that `form` may have; see `expand-all` if that's what you want."
  [form macros]
  (loop [original form
         expanded (expand-once form macros)]
    (if (= original expanded)
      original
      (recur expanded (expand-once expanded macros)))))

(defn expand-all
  [form macros]
  "Recursively expands `form` and its children, first expanding `form` itself
  using `expand`, then (if the result is a sequential form) expanding each
  child of the result using `expand-all`."
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
  "Propagates `syntax-quote` over a collection of `forms`. Typically, `forms`
  is actually a sequential form taken from within an outer syntax-quoted form.
  Effectively a slight modification of `syntax-quote`'s behavior to expand
  unquote-splice forms in place rather than erroring when one is encountered."
  [forms]
  (map (fn [form]
         (condp #(%1 %2) form
           unquote? [(second form)]
           unquote-splice? (list 'vec (second form))
           [(syntax-quote form)]))
       forms))

(defn syntax-quote
  "Recursively expands syntax-quoted form `form`, resolving internal unquote
  and unquote-splice forms and propagating ordinary quotation to other internal
  forms that have not been explicitly unquoted."
  [form]
  (condp #(%1 %2) form
    symbol? (list 'quote form)
    unquote? (second form)
    unquote-splice? (raise "invalid location for ~@" form)
    (some-fn (complement coll?) empty?) form
    list? (list 'apply 'list (cons 'concat (expand-sequence form)))
    map? (list 'apply 'hash-map
                      (cons 'concat (expand-sequence (apply concat form))))
    set? (list 'set (cons 'concat (expand-sequence form)))
    vector? (cons 'concat (expand-sequence form))
    (raise "unknown collection type" form)))
