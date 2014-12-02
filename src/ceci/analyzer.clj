(ns ceci.analyzer)

(defmacro with-state [state & body]
  `(binding [ceci.analyzer/*state* ~state] ~@body))
