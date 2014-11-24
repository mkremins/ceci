(ns ceci.reader
  (:refer-clojure :exclude [*data-readers* read-string])
  (:require [ceci.util :refer [mapk merge-meta metadatable? update]]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn reader-error [msg]
  (throw (js/Error. (str "ReaderError: " msg))))

;; underlying reader data structure

(defn line-num [reader]
  (+ (:line reader) (:line-offset reader) 1))

(defn col-num [reader]
  (+ (:column reader)
     (when (= (:line reader) 0) (:column-offset reader))
     1))

(defn make-reader [source]
  {:lines (->> (str/split source #"\n")
               (mapv #(conj (mapv str %) "\n")))
   :line 0 :column 0})

(defn make-nested-reader [parent-reader source]
  (let [{:keys [line line-offset column column-offset]} parent-reader]
    (assoc (make-reader source)
      :line-offset (+ line line-offset)
      :column-offset (+ column (when (= line 0) column-offset)))))

;; basic reader interface

(defn advance [{:keys [lines line column] :as reader}]
  (if (get-in lines [line (inc column)])
    (update reader :column inc)
    (-> reader (update :line inc) (assoc :column 0))))

(defn lookahead [num-chars reader]
  (loop [{:keys [lines line column] :as reader} reader
         chars-advanced 0]
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

(declare read-all-forms)

(defn read-between [opener closer reader]
  (let [original-reader (advance reader)]
    (loop [reader reader
           buffer opener
           nesting-level 0]
      (let [reader (advance reader)
            ch (curr-ch reader)
            buffer (str buffer ch)]
        (condp = ch
          opener
            (recur reader buffer (inc nesting-level))
          closer
            (if (= nesting-level 0)
              (let [code (->> buffer rest drop-last str/join)]
                [(advance reader)
                 (read-all-forms (make-nested-reader original-reader code))])
              (recur reader buffer (dec nesting-level)))
          nil
            (reader-error (str "unmatched delimiter " opener))
          ;else
            (recur reader buffer nesting-level))))))

(defn read-list [reader]
  (let [[reader children] (read-between "(" ")" reader)]
    [reader (apply list children)]))

(defn read-vector [reader]
  (let [[reader children] (read-between "[" "]" reader)]
    [reader (vec children)]))

(defn read-map [reader]
  (let [[reader children] (read-between "{" "}" reader)]
    [reader (apply hash-map children)]))

(defn read-set [reader]
  (let [[reader children] (read-between "{" "}" reader)]
    [reader (set children)]))

;; string and regex forms

(defn read-string [reader]
  (loop [reader reader
         buffer ""
         escape-next false]
    (let [reader (advance reader)
          ch (curr-ch reader)]
      (if escape-next
        (recur reader (str buffer ch) false)
        (case ch
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
  (let [[numerator denominator] (map parse-int (str/split s #"/" 2))]
    (when (and numerator denominator)
      (float (/ numerator denominator)))))

(defn parse-symbol [s]
  (case s
    "true" true
    "false" false
    "nil" nil
    (let [[ns-part name-part] (str/split s #"/" 2)]
      (if name-part (symbol ns-part name-part) (symbol ns-part)))))

(defn read-symbol-or-number [reader]
  (let [[reader buffer] (read-token reader)]
    [reader (or (parse-int buffer)
                (parse-float buffer)
                (parse-ratio buffer)
                (parse-symbol buffer))]))

;; anonymous functions

(defn find-anon-fn-args [form]
  (->> (tree-seq coll? identity form)
       (filter #(and (symbol? %) (= (first (str %)) "%")))
       distinct))

(defn expand-anon-fn-arg [form]
  (let [arg-name (subs (str form) 1)
        arg-name (if (= arg-name "") "1" arg-name)]
    (when-not (or (parse-int arg-name) (= arg-name "&"))
      (reader-error (str "invalid anonymous function argument form " form)))
    (gensym (if (= arg-name "&") "rest__" (str "p" arg-name "__")))))

(defn make-anon-fn-params [args-map]
  (let [params (->> args-map
                    (mapk #(subs (str %) 1))
                    (mapk #(if (= % "") "1" %))
                    (mapk parse-int)
                    (mapk (fnil dec Infinity))
                    (into {}))
        rest-param (get params Infinity)
        params (if rest-param (dissoc params Infinity) params)
        last-idx (apply max (keys params))]
    (if (nil? last-idx)
      (if rest-param ['& rest-param] [])
      (loop [params-form [] idx 0]
        (let [param (get params idx)]
          (cond
            (= idx last-idx)
              (let [params-form (conj params-form param)]
                (if rest-param
                  (-> params-form (conj '&) (conj rest-param))
                  params-form))
            param
              (recur (conj params-form param) (inc idx))
            :else
              (recur (conj params-form '_) (inc idx))))))))

(defn read-anon-fn [reader]
  (let [[reader list-form] (read-list reader)
        arg-syms (find-anon-fn-args list-form)
        args-map (zipmap arg-syms (map expand-anon-fn-arg arg-syms))
        body-form (walk/prewalk-replace args-map list-form)
        params-form (make-anon-fn-params args-map)]
    [reader (list 'fn* params-form body-form)]))

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
    [reader (list (if splicing? 'unquote-splicing 'unquote) form)]))

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

;; metadata

(defn read-meta [reader]
  (let [[reader meta-form] (read-next-form (advance reader))
        metadata (cond (map? meta-form) meta-form
                       (symbol? meta-form) {:tag meta-form}
                       (keyword? meta-form) {meta-form true}
                       :else (reader-error "invalid metadata shorthand"))
        [reader form] (read-next-form reader)]
    (when-not (metadatable? form)
      (reader-error "only symbols and collections support metadata"))
    [reader (vary-meta form merge metadata)]))

;; generic interface

(defn read-dispatch [reader]
  (case (curr-ch reader)
    "(" (read-anon-fn reader)
    "{" (read-set reader)
    "\"" (read-regex reader)
    "'" (read-var reader)
    "_" (read-discard reader)
    (read-tagged-literal reader)))

(defn read-next-form [reader]
  (when-let [ch (curr-ch reader)]
    (if (whitespace? ch)
      (read-whitespace reader)
      (case ch
        "(" (read-list reader)
        "[" (read-vector reader)
        "{" (read-map reader)
        ")" (reader-error "unmatched delimiter )")
        "]" (reader-error "unmatched delimiter ]")
        "}" (reader-error "unmatched delimiter }")
        "\"" (read-string reader)
        ":" (read-keyword reader)
        ";" (read-comment reader)
        "@" (read-deref reader)
        "'" (read-quote reader)
        "`" (read-syntax-quote reader)
        "~" (read-unquote reader)
        "^" (read-meta reader)
        "#" (read-dispatch (advance reader))
        (read-symbol-or-number reader)))))

(defn read-with-source-info [reader]
  (let [[reader _] (advance-while whitespace? reader)
        line (line-num reader)
        column (col-num reader)]
    (when-let [[reader form] (read-next-form reader)]
      [reader (merge-meta form {:line line :column column})])))

(defn read-all-forms [reader]
  (loop [reader reader forms []]
    (if-let [[reader form] (read-with-source-info reader)]
      (recur reader (conj forms form))
      forms)))

(defn read-code [source]
  (read-all-forms (make-reader source)))
