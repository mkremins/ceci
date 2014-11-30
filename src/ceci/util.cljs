(ns ceci.util)

(defn mapk [f m]
  (map (fn [[k v]] [(f k) v]) m))

(defn update [m k f & args]
  (apply update-in m [k] f args))

(def metadatable? (some-fn coll? symbol?))

(defn merge-meta [form metadata]
  (if (metadatable? form)
    (vary-meta form merge metadata)
    form))

(defn raise
  ([msg] (throw (js/Error. msg)))
  ([msg form]
    (raise (str msg "\nin form: " (pr-str form) " ("
                (if-let [{:keys [line column]} (meta form)]
                  (str line ":" column) "source missing")
                ")"))))
