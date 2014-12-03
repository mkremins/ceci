(ns ceci.util)

(defn mapk [f m]
  (map (fn [[k v]] [(f k) v]) m))

(defn update [m k f & args]
  (apply update-in m [k] f args))

(defn name* [sym]
  (symbol (name sym)))

(defn ns* [sym]
  (when-let [ns (namespace sym)] (symbol ns)))

(defn symbol* [ns name]
  (symbol (str ns) (str name)))

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
