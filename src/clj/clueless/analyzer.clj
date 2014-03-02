(ns clueless.analyzer)

(defn analyzer-error [msg]
  (throw (RuntimeException. msg)))

(defn analyze-def
  ([name] (analyze-def name {:type :nil}))
  ([name init-val]
    (assert (= (:type name) :symbol))
    {:type :def :name (:value name) :value init-val}))

(defn analyze-do [body]
  {:type :do :body body})

(defn compile-bindings [bindings]
  (if (= (:type bindings) :vector)
    (let [pairs (partition 2 (:children bindings))]
      (if (= (count (last pairs)) 2)
        (vec (map (juxt (comp :value first) second) pairs))
        (analyzer-error "number of forms in bindings vector must be even")))
    (analyzer-error "bindings form must be vector")))

(defn analyze-let [bindings body]
  {:type :let :bindings (compile-bindings bindings) :body body})

(defn analyze-if
  ([expr then-clause] (analyze-if expr then-clause {:type :nil}))
  ([expr then-clause else-clause]
    {:type :if :expr expr :then then-clause :else else-clause}))

(defn analyze-js* [js-string]
  {:type :js* :value js-string})

;; fn forms

(def last-fnid (atom 0))

(defn make-fname []
  (str "fn_" (swap! last-fnid inc)))

(defn analyze-params [{:keys [children]}]
  (vec (map :value children)))

(defn analyze-clause-dispatch [clauses]
  (let [clauses (map (fn [{:keys [children] :as clause}]
                       (-> clause
                         (assoc :params (analyze-params (first children)))
                         (assoc :body (rest children))
                         (dissoc :children)))
                      clauses)]
    (zipmap (map #(count (:params %)) clauses) clauses)))

(defn analyze-multi-clause-fn [name clauses]
  {:type :fn :name name
   :clauses (analyze-clause-dispatch clauses)})

(defn analyze-single-clause-fn [name params body]
  (analyze-multi-clause-fn name [{:children (concat [params] (vec body))}]))

(defn analyze-named-fn [name args]
  (condp = (:type (first args))
    :vector (analyze-single-clause-fn name (first args) (rest args))
    :list (analyze-multi-clause-fn name args)
    (analyzer-error "invalid function definition")))

(defn analyze-fn [args]
  (condp = (:type (first args))
    :symbol (analyze-named-fn (:value (first args)) (rest args))
    :vector (analyze-single-clause-fn (make-fname) (first args) (rest args))
    :list (analyze-multi-clause-fn (make-fname) args)
    (analyzer-error "invalid function definition")))

;; generic interface

(defn analyze-special-form [{:keys [children type] :as ast-node}]
  (when (= type :list)
    (let [first-child (first children)]
      (when (= (:type first-child) :symbol)
        (condp = (:value first-child)
          "def" (analyze-def (second children) (get children 2))
          "do" (analyze-do (rest children))
          "let" (analyze-let (second children) (drop 2 children))
          "fn" (analyze-fn (rest children))
          "if" (analyze-if (second children) (get children 2) (get children 3))
          "js*" (analyze-js* (second children))
          nil)))))

(defn analyze-literal-constant [{:keys [type value] :as ast-node}]
  (when (= type :symbol)
    (condp = value
      "nil" {:type :nil}
      "true" {:type :bool :value true}
      "false" {:type :bool :value false}
      nil)))

(defn analyze-map [{:keys [type children] :as ast-node}]
  (when (= type :map)
    (let [pairs (->> children (partition 2) (map vec) (vec))]
      (if (= (count pairs) (/ (count children) 2))
        {:type :map :pairs pairs}
        (analyzer-error "number of forms in map literal must be even")))))

(defn analyze [ast-node]
  (if-let [special-form (analyze-special-form ast-node)]
    special-form
    (if-let [literal-constant (analyze-literal-constant ast-node)]
      literal-constant
      (if-let [map-literal (analyze-map ast-node)]
        map-literal
        (if (contains? ast-node :children)
          (update-in ast-node [:children] #(vec (map analyze %)))
          ast-node)))))
