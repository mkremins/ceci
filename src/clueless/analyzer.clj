(ns clueless.analyzer)

(defn analyzer-error [msg]
  (throw (RuntimeException. msg)))

(defn expand-def
  ([name] (expand-def name {:type :nil}))
  ([name init-val]
    (assert (= (:type name) :symbol))
    {:type :def :name (:value name) :value init-val}))

(defn expand-do [body]
  {:type :do :body body})

(defn compile-bindings [bindings]
  (if (= (:type bindings) :vector)
    (let [pairs (partition 2 (:children bindings))]
      (if (= (count (last pairs)) 2)
        (vec (map (juxt (comp :value first) second) pairs))
        (analyzer-error "number of forms in bindings vector must be even")))
    (analyzer-error "bindings form must be vector")))

(defn expand-let [bindings body]
  {:type :let :bindings (compile-bindings bindings) :body body})

(defn expand-if
  ([expr then-clause] (expand-if expr then-clause {:type :nil}))
  ([expr then-clause else-clause]
    {:type :if :expr expr :then then-clause :else else-clause}))

(defn expand-js* [js-string]
  (assert (= (:type js-string) :string))
  {:type :js* :value (:value js-string)})

;; fn forms

(def last-fnid (atom 0))

(defn make-fname []
  (str "fn_" (swap! last-fnid inc)))

(defn expand-params [{:keys [children]}]
  (vec (map :value children)))

(defn expand-clause-dispatch [clauses]
  (let [clauses (map (fn [{:keys [children] :as clause}]
                       (-> clause
                         (assoc :params (expand-params (first children)))
                         (assoc :body (rest children))
                         (dissoc :children)))
                      clauses)]
    (zipmap (map #(count (:params %)) clauses) clauses)))

(defn expand-multi-clause-fn [name clauses]
  {:type :fn :name name
   :clauses (expand-clause-dispatch clauses)})

(defn expand-single-clause-fn [name params body]
  (expand-multi-clause-fn name [{:children params :body body}]))

(defn expand-named-fn [name args]
  (condp = (:type (first args))
    :vector (expand-single-clause-fn name (first args) (rest args))
    :list (expand-multi-clause-fn name args)
    (analyzer-error "invalid function definition")))

(defn expand-fn [args]
  (condp = (:type (first args))
    :symbol (expand-named-fn (:value (first args)) (rest args))
    :vector (expand-single-clause-fn (make-fname) (first args) (rest args))
    :list (expand-multi-clause-fn (make-fname) args)
    (analyzer-error "invalid function definition")))

;; generic interface

(defn expand-special-form [{:keys [children type] :as ast-node}]
  (when (= type :list)
    (let [first-child (first children)]
      (when (= (:type first-child) :symbol)
        (condp = (:value first-child)
          "def" (expand-def (second children) (get children 2))
          "do" (expand-do (rest children))
          "let" (expand-let (second children) (drop 2 children))
          "fn" (expand-fn (rest children))
          "if" (expand-if (second children) (get children 2) (get children 3))
          "js*" (expand-js* (second children))
          nil)))))

(defn expand-literal-constant [{:keys [type value] :as ast-node}]
  (when (= type :symbol)
    (condp = value
      "nil" {:type :nil}
      "true" {:type :bool :value true}
      "false" {:type :bool :value false}
      nil)))

(defn expand-map [{:keys [type children] :as ast-node}]
  (when (= type :map)
    (let [pairs (->> children (partition 2) (map vec) (vec))]
      (if (= (count pairs) (/ (count children) 2))
        {:type :map :pairs pairs}
        (analyzer-error "number of forms in map literal must be even")))))

(defn expand [ast-node]
  (if-let [special-form (expand-special-form ast-node)]
    special-form
    (if-let [literal-constant (expand-literal-constant ast-node)]
      literal-constant
      (if-let [map-literal (expand-map ast-node)]
        map-literal
        ast-node))))
