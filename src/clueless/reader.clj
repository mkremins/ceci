(ns clueless.reader
  (:require [clojure.string :as string])
  (:refer-clojure :exclude [read-string]))

(defn reader-error [msg]
  (throw (RuntimeException. msg)))

(defn make-reader [source]
  {:lines (->> (string/split source #"\n")
            (map (comp vec (partial map str)))
            (map #(conj % "\n"))
            (vec))
   :line 0 :column 0})

(defn advance [{:keys [lines line column] :as reader}]
  (if (get-in lines [line (inc column)])
    (update-in reader [:column] inc)
    (-> reader (update-in [:line] inc) (assoc :column 0))))

(defn lookahead [num-chars reader]
  (loop [{:keys [lines line column] :as reader} reader chars-advanced 0]
    (if (= chars-advanced num-chars)
      (get-in lines [line column])
      (recur (advance reader) (inc chars-advanced)))))

(def curr-ch (partial lookahead 0))
(def next-ch (partial lookahead 1))

(defn advance-while [pred reader]
  (loop [reader reader buffer ""]
    (let [ch (curr-ch reader)]
      (if (and ch (pred ch))
        (recur (advance reader) (str buffer ch))
        [reader buffer]))))

(def whitespace? #{" " "\n" "\r" "\t" ","})

;; braced forms (list, vector, map)

(def matching-delimiter
  {"(" ")" "[" "]" "{" "}"})

(def form-type
  {"(" :list "[" :vector "{" :map})

(defn read-delimited-form
  ([l-delim reader]
    (read-delimited-form l-delim (matching-delimiter l-delim)
                      (form-type l-delim) reader))
  ([l-delim r-delim type reader]
    (loop [reader reader buffer l-delim nesting-level 0]
      (let [reader (advance reader)
            ch (curr-ch reader)
            buffer (str buffer ch)]
        (condp = ch
          l-delim (recur reader buffer (inc nesting-level))
          r-delim (if (= nesting-level 0)
                    [(advance reader) {:type type :source buffer}]
                    (recur reader buffer (dec nesting-level)))
          nil (reader-error (str "unmatched delimiter " l-delim))
          (recur reader buffer nesting-level))))))

(def read-list (partial read-delimited-form "("))
(def read-vector (partial read-delimited-form "["))
(def read-map (partial read-delimited-form "{"))

;; string forms

(defn read-string [reader]
  (loop [reader reader buffer "" escape-next false]
    (let [reader (advance reader)
          ch (curr-ch reader)]
      (if escape-next
        (recur reader (str buffer ch) false)
        (condp = ch
          "\\" (recur reader buffer true)
          "\"" [reader {:type :string :value buffer}]
          (recur reader (str buffer ch) false))))))

;; keyword, number and symbol forms

(defn read-token [reader]
  (advance-while (complement whitespace?) reader))

(defn read-keyword [reader]
  (let [[reader buffer] (read-token (advance reader))]
    [reader {:type :keyword :value buffer}]))

(defn parse-int [s]
  (try (Integer/parseInt s)
    (catch NumberFormatException e
      nil)))

(defn parse-float [s]
  (try (Float/parseFloat s)
    (catch NumberFormatException e
      nil)))

(defn read-int [s]
  (when-let [int-value (parse-int s)]
    {:type :number :value int-value}))

(defn read-float [s]
  (when-let [float-value (parse-float s)]
    {:type :number :value float-value}))

(defn read-symbol [s]
  {:type :symbol :value s})

(defn read-symbol-or-number [reader]
  (let [[reader buffer] (read-token reader)]
    [reader (or (read-int buffer)
                (read-float buffer)
                (read-symbol buffer))]))

;; whitespace and comment forms

(declare read-next-form)

(defn read-comment [reader]
  (read-next-form (first (advance-while #(not= % "\n") reader))))

(defn read-whitespace [reader]
  (read-next-form (first (advance-while whitespace? reader))))

;; generic interface

(defn read-next-form [reader]
  (when-let [ch (curr-ch reader)]
    (if (whitespace? ch)
      (read-whitespace reader)
      (condp = ch
        "(" (read-list reader)
        "[" (read-vector reader)
        "{" (read-map reader)
        "\"" (read-string reader)
        ":" (read-keyword reader)
        ";" (read-comment reader)
        ;"'" (read-quoted-form reader)
        ;"`" (read-syntax-quoted-form reader)
        ;"^" (read-meta reader)
        ;"#" (condp = (next-ch reader)
        ;      "(" (read-anon-fn reader)
        ;      "{" (read-set reader)
        ;      "\"" (read-regex reader)
        ;      (read-tagged-literal reader))
        (read-symbol-or-number reader)))))

(defn read-all-forms [reader]
  (loop [reader reader forms []]
    (if-let [[reader form] (read-next-form reader)]
      (recur reader (conj forms form))
      forms)))

(defn expand-ast-node [{:keys [type source] :as ast-node}]
  (if (and (#{:list :vector :map} type) source)
    (let [reader (make-reader (->> source (drop 1) (drop-last) (string/join)))]
      (assoc ast-node :children (read-all-forms reader)))
    ast-node))

(defn read-code [source]
  (->> (make-reader source)
    (read-all-forms)
    (map expand-ast-node)))
