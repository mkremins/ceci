(ns ceci.util)

(defn update [m k f & args]
  (apply (partial update-in m [k] f) args))

;; metadata helpers

(defn metadatable? [form]
  (or (coll? form) (symbol? form)))

(defn merge-meta [form metadata]
  (if (metadatable? form)
      (vary-meta form merge metadata)
      form))

;; error reporting

(defn raise-missing-source [message form]
  (throw (js/Error. (str "Compilation error: " message "\n"
                         "in form: " (pr-str form)
                         " (source missing)"))))

(defn raise
  ([message form]
    (if-let [{:keys [line column]} (meta form)]
      (raise message form line column)
      (raise-missing-source message form)))
  ([message form line column]
    (throw (js/Error. (str "Compilation error: " message "\n"
                           "in form: " (pr-str form)
                           " (" line ":" column ")")))))
