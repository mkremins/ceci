(ns clueless.util)

(defn update [m k f & args]
  (apply (partial update-in m [k] f) args))

;; metadata helpers

(defn metadatable? [form]
  (or (coll? form) (symbol? form)))

(defn merge-meta [form metadata]
  (if (metadatable? form)
      (vary-meta form merge metadata)
      form))
