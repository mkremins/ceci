(ns clueless.reader
  (:require [clojure.string :as string])
  (:refer-clojure :exclude [*data-readers* read-string]))

(defn reader-error [msg]
  (throw (js/Error. (str "ReaderError: " msg))))

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

;; delimited forms (list, vector, map, set)

(def matching-delimiter
  {"(" ")" "[" "]" "{" "}"})

(declare read-code)

(defn read-delimited-forms
  ([l-delim reader]
    (read-delimited-forms l-delim (matching-delimiter l-delim) reader))
  ([l-delim r-delim reader]
    (loop [reader reader buffer l-delim nesting-level 0]
      (let [reader (advance reader)
            ch (curr-ch reader)
            buffer (str buffer ch)]
        (condp = ch
          l-delim (recur reader buffer (inc nesting-level))
          r-delim (if (= nesting-level 0)
                    (let [code (->> buffer (drop 1) (drop-last) (string/join))]
                      [(advance reader) (read-code code)])
                    (recur reader buffer (dec nesting-level)))
          nil (reader-error (str "unmatched delimiter " l-delim))
          (recur reader buffer nesting-level))))))

(defn read-list [reader]
  (let [[reader children] (read-delimited-forms "(" reader)]
    [reader (apply list children)]))

(defn read-vector [reader]
  (let [[reader children] (read-delimited-forms "[" reader)]
    [reader (vec children)]))

(defn read-map [reader]
  (let [[reader children] (read-delimited-forms "{" reader)]
    [reader (apply hash-map children)]))

(defn read-set [reader]
  (let [[reader children] (read-delimited-forms "{" reader)]
    [reader (set children)]))

;; string and regex forms

(defn read-string [reader]
  (loop [reader reader buffer "" escape-next false]
    (let [reader (advance reader)
          ch (curr-ch reader)]
      (if escape-next
        (recur reader (str buffer ch) false)
        (condp = ch
          "\"" [(advance reader) buffer]
          "\\" (recur reader buffer true)
          nil (reader-error "unmatched delimiter \"")
          (recur reader (str buffer ch) false))))))

(defn read-regex [reader]
  (let [[reader value] (read-string reader)]
    [reader (re-pattern value)]))

;; keyword, number and symbol forms

(defn read-token [reader]
  (advance-while (complement whitespace?) reader))

(defn read-keyword [reader]
  (let [[reader buffer] (read-token (advance reader))]
    [reader (keyword buffer)]))

(defn parse-int [s]
  (let [int-pattern #"^(\-|\+)?([0-9]+|Infinity)$"]
    (when (.test int-pattern s)
      (js/Number s))))

(defn parse-float [s]
  (let [float-pattern #"^(\-|\+)?([0-9]+(\.[0-9]+)?|Infinity)$"]
    (when (.test float-pattern s)
      (js/Number s))))

(defn parse-ratio [s]
  (when-let [[numerator denominator] (string/split s #"/")]
    (let [numerator (parse-int numerator)
          denominator (parse-int denominator)]
      (when (and numerator denominator)
        (float (/ numerator denominator))))))

(defn parse-symbol [s]
  (symbol s))

(defn read-symbol-or-number [reader]
  (let [[reader buffer] (read-token reader)]
    [reader (or (parse-int buffer)
                (parse-float buffer)
                (parse-ratio buffer)
                (parse-symbol buffer))]))

;; whitespace and comment forms

(declare read-next-form)

(defn read-comment [reader]
  (read-next-form (first (advance-while #(not= % "\n") reader))))

(defn read-whitespace [reader]
  (read-next-form (first (advance-while whitespace? reader))))

(defn read-discard [reader]
  (read-next-form (first (read-next-form (advance reader)))))

;; quotation-related forms

(defn read-wrapped [type reader]
  (let [[reader form] (read-next-form (advance reader))]
    [reader (list type form)]))

(def read-deref (partial read-wrapped 'deref))
(def read-quote (partial read-wrapped 'quote))
(def read-syntax-quote (partial read-wrapped 'syntax-quote))

(defn read-unquote [reader]
  (let [splicing? (= (next-ch reader) "@")
        reader (if splicing? (-> reader advance advance) (advance reader))
        [reader form] (read-next-form reader)]
    [reader (list (if splicing? 'unquote-splice 'unquote) form)]))

(defn read-var [reader]
  (let [[reader form] (read-next-form (advance reader))]
    (when-not (symbol? form)
              (reader-error "can only use var syntax #' with a symbol"))
    [reader (list 'var form)]))

;; tagged literals

(def ^:dynamic *data-readers* {})

(defn read-tagged-literal [reader]
  (let [[reader tag] (read-token reader)
        [reader form] (read-next-form reader)]
    (if-let [tag-parser (get *data-readers* tag)]
      [reader (tag-parser form)]
      (reader-error (str "no registered parser for tag " tag)))))

;; generic interface

(defn read-dispatch [reader]
  (condp = (curr-ch reader)
;    "(" (read-anon-fn reader)
    "{" (read-set reader)
    "\"" (read-regex reader)
    "'" (read-var reader)
    "_" (read-discard reader)
    (read-tagged-literal reader)))

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
        "@" (read-deref reader)
        "'" (read-quote reader)
        "`" (read-syntax-quote reader)
        "~" (read-unquote reader)
;        "^" (read-meta reader)
        "#" (read-dispatch (advance reader))
        (read-symbol-or-number reader)))))

(defn read-all-forms [reader]
  (loop [reader reader forms []]
    (if-let [[reader form] (read-next-form reader)]
      (recur reader (conj forms form))
      forms)))

(defn read-code [source]
  (read-all-forms (make-reader source)))
