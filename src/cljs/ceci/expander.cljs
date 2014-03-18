(ns ceci.expander
  (:refer-clojure :exclude [defmacro])
  (:require [ceci.analyzer :as ana]
            [ceci.emitter :as emitter]
            [ceci.util :refer [merge-meta metadatable? raise]]))

;; macro management

(def macros (atom {}))

(defn install-macro! [macro-name macro]
  (swap! macros assoc macro-name macro))

(defn expand-macro
  "Assumes `form` is a list form. If `(first form)` names a macro that has been
  installed using `install-macro!`, retrieves the named macro and uses it to
  expand `form`, returning the result. Otherwise, returns `form`."
  [form]
  (if-let [expander (get @macros (first form))]
    (let [metadata (meta form)]
      (merge-meta (apply expander (rest form)) metadata))
    form))

;; syntax sugar

(defn desugar-new-syntax
  "Assumes `form` is a list form. If `(first form)` ends with a period,
  desugars `form` to the canonical constructor-invoke syntax `(new ctor args)`
  and returns the result. Otherwise, returns `form`."
  [form]
  (let [ctor-name (str (first form))]
    (if (= (last ctor-name) ".")
        (apply list 'new
               (symbol (subs ctor-name 0 (dec (count ctor-name))))
               (rest form))
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
  [form]
  (if (and (list? form) (symbol? (first form)))
      (-> form
          expand-macro
          desugar-new-syntax
          desugar-field-access
          desugar-method-call)
      form))

(defn expand
  "Expands `form` repeatedly (using `expand-once`) until the result no longer
  represents a macro form, then returns the result. Does not recursively expand
  any children that `form` may have; see `expand-all` if that's what you want."
  [form]
  (loop [original form
         expanded (expand-once form)]
    (if (= original expanded)
        original
        (recur expanded (expand-once expanded)))))

(defn expand-all
  [form]
  "Recursively expands `form` and its children, first expanding `form` itself
  using `expand`, then (if the result is a sequential form) expanding each
  child of the result using `expand-all`."
  (let [form (expand form)
        metadata (meta form)
        expanded
        (cond (list? form) (apply list (map expand-all form))
              (map? form) (apply hash-map (map expand-all (apply concat form)))
              (set? form) (set (map expand-all form))
              (vector? form) (vec (map expand-all form))
              :else form)]
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
         (cond (unquote? form) [(second form)]
               (unquote-splice? form) (list 'vec (second form))
               :else [(syntax-quote form)]))
       forms))

(defn syntax-quote
  "Recursively expands syntax-quoted form `form`, resolving internal unquote
  and unquote-splice forms and propagating ordinary quotation to other internal
  forms that have not been explicitly unquoted."
  [form]
  (cond (symbol? form) (list 'quote form)
        (unquote? form) (second form)
        (unquote-splice? form) (raise "invalid location for ~@" form)
        (or (not (coll? form)) (empty? form)) form
        (list? form) (list 'apply 'list (cons 'concat (expand-sequence form)))
        (map? form) (list 'apply 'hash-map
                          (cons 'concat (expand-sequence (apply concat form))))
        (set? form) (list 'set (cons 'concat (expand-sequence form)))
        (vector? form) (cons 'concat (expand-sequence form))
        :else (raise "unknown collection type" form)))

(install-macro! 'syntax-quote syntax-quote)

;; defmacro

(defn defmacro [name & args]
  (let [fn-form (expand-all (cons 'fn* args))
        compiled (->> fn-form
                      ana/form->ast
                      (ana/analyze {:context :expr :locals [] :quoted? false})
                      emitter/emit)
        macro (js/eval compiled)]
    (install-macro! name macro)
    (list 'def name fn-form)))

(install-macro! 'defmacro defmacro)
