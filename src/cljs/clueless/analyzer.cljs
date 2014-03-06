(ns clueless.analyzer)

(defn analyzer-error [msg]
  (throw (js/Error. (str "AnalyzerError: " msg))))

;; AST creation

(defn node-type [form]
  (cond (keyword? form) :keyword
        (list? form) :list
        (map? form) :map
        (nil? form) :nil
        (number? form) :number
        (set? form) :set
        (string? form) :string
        (symbol? form) :symbol
        (vector? form) :vector
        :else :IDK))

(defn form->ast [form]
  (let [type (node-type form)
        ast {:type type :form form}]
    (if (coll? form)
        (-> ast (assoc :op :coll) (assoc :children (map form->ast form)))
        (-> ast (assoc :op :const)))))

;; def, do, if forms

(defn analyze-def [{[_ name & [init?]] :children :as ast}]
  (-> ast
    (assoc :op :def)
    (assoc :name name)
    (assoc :init (or init? {:op :const :type :nil :form nil}))))

(defn analyze-do [{[_ & body] :children :as ast}]
  (-> ast
    (assoc :op :do)
    (assoc :body body)))

(defn analyze-if [{[_ test then else] :children :as ast}]
  (-> ast
    (assoc :op :if)
    (assoc :test test)
    (assoc :then then)
    (assoc :else else)))

(defn analyze-quote [{[_ quoted] :children :as ast}]
  (-> ast
    (assoc :op :quote)
    (assoc :quoted quoted)))

;; fn forms

(def last-fnid (atom 0))

(defn make-fname []
  {:op :const :type :symbol
   :form (symbol (str "fn_" (swap! last-fnid inc)))})

(defn analyze-params [{:keys [children]}]
  (vec (map :form children)))

(defn analyze-clause-dispatch [clauses]
  (let [clauses (map (fn [{:keys [children] :as clause}]
                       (-> clause
                         (assoc :params (analyze-params (first children)))
                         (assoc :body (rest children))
                         (dissoc :children)))
                      clauses)]
    (zipmap (map #(count (:params %)) clauses) clauses)))

(defn analyze-multi-clause-fn [name clauses]
  {:op :fn :name name
   :clauses (analyze-clause-dispatch clauses)})

(defn analyze-single-clause-fn [name params body]
  (analyze-multi-clause-fn name [{:children (concat [params] (vec body))}]))

(defn analyze-named-fn [name args]
  (condp = (:type (first args))
    :vector (analyze-single-clause-fn name (first args) (rest args))
    :list (analyze-multi-clause-fn name args)
    (analyzer-error "invalid function definition")))

(defn analyze-fn [{[_ & args] :children :as ast}]
  (condp = (:type (first args))
    :symbol (analyze-named-fn (first args) (rest args))
    :vector (analyze-single-clause-fn (make-fname) (first args) (rest args))
    :list (analyze-multi-clause-fn (make-fname) args)
    (analyzer-error "invalid function definition")))

;; let forms

(defn compile-bindings [bindings]
  (if (= (:type bindings) :vector)
    (let [pairs (partition 2 (:children bindings))]
      (if (= (count (last pairs)) 2)
        (vec (map (juxt (comp :form first) second) pairs))
        (analyzer-error "number of forms in bindings vector must be even")))
    (analyzer-error "bindings form must be vector")))

(defn analyze-let [{[_ bindings & body] :children :as ast}]
  (-> ast
    (assoc :op :let)
    (assoc :bindings (compile-bindings bindings))
    (assoc :body body)))

;; AST analysis

(declare analyze)

(def specials
  {'def analyze-def
   'do  analyze-do
   'fn  analyze-fn
   'if  analyze-if
   'let analyze-let
   'quote analyze-quote})

(defn analyze-list [{:keys [form] :as ast}]
  (if-let [head (first form)]
    (cond (symbol? head)
          (if-let [analyze-special (specials head)]
            (analyze-special ast)
            ast)
          :else ast)))

(defn analyze-coll [ast]
  (update-in ast [:children] (partial map analyze)))

(defn analyze-symbol [{sym :form :as ast}]
  (condp = sym
    'true (-> ast (assoc :type :bool) (assoc :form true))
    'false (-> ast (assoc :type :bool) (assoc :form false))
    'nil (-> ast (assoc :type :nil) (assoc :form nil))
    ast))

(defn analyze [{:keys [op type] :as ast}]
  (if (= op :coll)
      (if (= type :list)
          (analyze-list (analyze-coll ast))
          (analyze-coll ast))
      (if (= type :symbol)
          (analyze-symbol ast)
          ast)))
