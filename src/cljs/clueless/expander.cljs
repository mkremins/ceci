(ns clueless.expander)

(defn expander-error [msg]
  (throw (js/Error. (str "ExpanderError: " msg))))

(def macros (atom {}))

;; metadata helpers

(defn metadatable? [form]
  (or (coll? form) (symbol? form)))

(defn safe-merge-meta [form metadata]
  (if (metadatable? form)
      (vary-meta form merge metadata)
      form))

;; public API

(defn expand-once
  "Expands `form` once. If `form` is a list with a registered macro's name as
  its first element, applies the corresponding macro to the remaining elements
  of `form` and returns the result; otherwise, returns `form`."
  [form]
  (if (list? form)
      (if-let [expander (get @macros (first form))]
              (let [metadata (meta form)]
                (safe-merge-meta (apply expander (rest form)) metadata))
              form)
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
    (safe-merge-meta expanded metadata)))

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
        (unquote-splice? form) (expander-error "invalid location for ~@")
        (or (not (coll? form)) (empty? form)) form
        (list? form) (list 'apply 'list (cons 'concat (expand-sequence form)))
        (map? form) (list 'apply 'hash-map
                          (cons 'concat (expand-sequence (apply concat form))))
        (set? form) (list 'set (cons 'concat (expand-sequence form)))
        (vector? form) (cons 'concat (expand-sequence form))
        :else (expander-error "unknown collection type")))

(swap! macros assoc 'syntax-quote syntax-quote)
