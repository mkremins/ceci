(ns ceci.util)

(def metadatable? (some-fn coll? symbol?))

(defn merge-meta [form metadata]
  (if (metadatable? form)
    (vary-meta form merge metadata)
    form))

(defn source-info [form]
  (str "in form: " (pr-str form) " "
       (if-let [{:keys [line column]} (meta form)]
         (str "(" line ":" column ")") "(source missing)")))

(defn raise
  ([msg] (throw (js/Error. msg)))
  ([msg form] (raise (str msg "\n" (source-info form)))))

(defn warn
  ([msg] (println (str "Warning: " msg)))
  ([msg form] (warn (str msg "\n" (source-info form)))))

(defn take-when [pred [x & xs :as coll]]
  (if (pred x) [x xs] [nil coll]))
