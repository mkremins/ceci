(ns ceci.reader
  (:refer-clojure :exclude [*data-readers* read-string])
  (:require [ceci.util :refer [merge-meta metadatable?]]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn reader-error [msg]
  (throw (js/Error. (str "ReaderError: " msg))))

;; underlying reader data structure

(defn make-reader [source]
  {:lines (->> (str/split source #"\n") (mapv #(conj (mapv str %) "\n")))
   :line 0 :column 0})

(defn source-info [{:keys [line line-offset column column-offset]}]
  {:line (+ line line-offset 1)
   :column (+ column (when (zero? line) column-offset) 1)})

(defn make-nested-reader [parent-reader source]
  (let [{:keys [line column]} (source-info parent-reader)]
    (assoc (make-reader source)
      :line-offset (dec line) :column-offset (dec column))))

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

(defn read-collection [opener wrap closer]
  (fn [reader]
    (let [[reader children] (read-between opener closer reader)]
      [reader (wrap children)])))

(def read-list (read-collection \( #(apply list %) \)))
(def read-map (read-collection \{ #(apply hash-map %) \}))
(def read-set (read-collection \{ set \}))
(def read-vector (read-collection \[ vec \]))

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
          \" [(advance reader) buffer]
          \\ (recur reader buffer true)
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
    (when (.test int-pattern s) (js/Number s))))

(defn parse-float [s]
  (let [float-pattern #"^(\-|\+)?([0-9]+(\.[0-9]+)?|Infinity)$"]
    (when (.test float-pattern s) (js/Number s))))

(defn parse-ratio [s]
  (let [[numerator denominator] (map parse-int (str/split s #"/" 2))]
    (when (and numerator denominator) (float (/ numerator denominator)))))

(defn parse-bool-nil-or-symbol [s]
  (case s "true" true "false" false "nil" nil
          (let [[ns-part name-part] (str/split s #"/" 2)]
            (if name-part (symbol ns-part name-part) (symbol ns-part)))))

(defn read-number-bool-nil-or-symbol [reader]
  (let [[reader buffer] (read-token reader)]
    [reader (or (parse-int buffer)
                (parse-float buffer)
                (parse-ratio buffer)
                (parse-bool-nil-or-symbol buffer))]))

;; #() anonymous function shorthand

(defn find-anon-fn-args [form]
  (->> (tree-seq coll? identity form)
       (filter #(and (symbol? %) (= (first (str %)) \%)))
       distinct))

(defn anon-fn-arg-id [sym]
  (let [arg (subs (str sym) 1)
        arg (if (empty? arg) "1" arg)]
    (or (some-> arg parse-int dec) (when (= arg "&") :rest))))

(defn expand-anon-fn-arg [sym]
  (if-let [arg (anon-fn-arg-id sym)]
    (gensym (if (= arg :rest) "rest__" (str "p" arg "__")))
    (reader-error (str "invalid anonymous function argument form " sym))))

(defn anon-fn-params [args-map]
  (let [params (zipmap (map anon-fn-arg-id (keys args-map)) (vals args-map))
        params-form (mapv params (range (count (dissoc params :rest))))]
    (cond-> params-form (:rest params) (into ['& (:rest params)]))))

(defn read-anon-fn [reader]
  (let [[reader list-form] (read-list reader)
        arg-syms (find-anon-fn-args list-form)
        args-map (zipmap arg-syms (map expand-anon-fn-arg arg-syms))]
    [reader `(fn* ~(anon-fn-params args-map)
                  ~(walk/prewalk-replace args-map list-form))]))

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
  (let [splicing? (= (next-ch reader) \@)
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
        metadata (condp #(%1 %2) meta-form
                   map? meta-form
                   symbol? {:tag meta-form}
                   keyword? {meta-form true}
                   (reader-error "invalid metadata shorthand"))
        [reader form] (read-next-form reader)]
    (when-not (metadatable? form)
      (reader-error "only symbols and collections support metadata"))
    [reader (vary-meta form merge metadata)]))

;; generic interface

(defn read-dispatch [reader]
  (case (curr-ch reader)
    \( (read-anon-fn reader)
    \{ (read-set reader)
    \" (read-regex reader)
    \' (read-var reader)
    \_ (read-discard reader)
    (read-tagged-literal reader)))

(defn read-next-form [reader]
  (when-let [ch (curr-ch reader)]
    (if (whitespace? ch)
      (read-whitespace reader)
      (case ch
        \( (read-list reader)
        \[ (read-vector reader)
        \{ (read-map reader)
        \) (reader-error "unmatched delimiter )")
        \] (reader-error "unmatched delimiter ]")
        \} (reader-error "unmatched delimiter }")
        \" (read-string reader)
        \: (read-keyword reader)
        \; (read-comment reader)
        \@ (read-deref reader)
        \' (read-quote reader)
        \` (read-syntax-quote reader)
        \~ (read-unquote reader)
        \^ (read-meta reader)
        \# (read-dispatch (advance reader))
        (read-number-bool-nil-or-symbol reader)))))

(defn read-with-source-info [reader]
  (let [[reader _] (advance-while whitespace? reader)]
    (when-let [[reader* form] (read-next-form reader)]
      [reader* (merge-meta form (source-info reader))])))

(defn read-all-forms [reader]
  (loop [reader reader forms []]
    (if-let [[reader form] (read-with-source-info reader)]
      (recur reader (conj forms form))
      forms)))

(defn read-code [source]
  (read-all-forms (make-reader source)))
