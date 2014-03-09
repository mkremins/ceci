(ns clueless.util)

(defn update [m k f & args]
  (apply (partial update-in m [k] f) args))
