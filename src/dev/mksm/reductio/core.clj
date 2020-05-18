(ns dev.mksm.reductio.core
  "Reductio is a library that allows partial evaluation of subset of Clojure code"
  (:gen-class)
  (:require [clojure.walk :as w]))

#_(require '[clojure.test :as t])

(defn code->node
  #_(t/are [x y] (= x y)
      (code->node 1) {:code 1}
      (code->node nil) {:code nil}
      (code->node {}) {:code {}}
      (code->node '()) {:code '()}
      (code->node '([])) {:code '({:code [] :final false}) :final false})
  [code]
  {:code (if (coll? code) (w/walk code->node identity code) code)})

(defn- encodable? [val]
  (or (nil? val) (boolean? val) (int? val) (float? val) (string? val)
      (keyword? val) (list? val) (map? val) (vector? val) (set? val)))

(defn- node->code
  #_(t/are [x y] (= x y)
      (node->code {:code nil}) nil
      (node->code {:code 1}) 1
      (node->code {:code '()}) '()
      (node->code {:code '({:code []})}) '([]))
  [{:keys [code val] :as node}]
  (if (and (contains? node :val)
           (encodable? val))
    val
    (if (coll? code)
      (w/walk node->code identity code)
      code)))

(defn- deval-node
  #_(t/are [i x y] (and i (= x y))
      1 (deval-node (code->node 1) {}) {:code 1 :val 1}
      2 (deval-node (code->node 'a) {}) {:code 'a}
      3 (deval-node (code->node 'a) {'a 1}) {:code 'a :val 1}
      4 (deval-node (code->node '()) {}) {:code '() :error :no-fn}
      5 (deval-node (code->node '(+ 1 2)) {'+ +})
      (assoc (code->node '(+ 1 2)) :val 3)
      6 (deval-node (code->node '(+ 1 a)) {'+ + 'a 1})
      (assoc (code->node '(+ 1 a)) :val 2))
  [{:keys [code] :as node} bindings]
  (cond (contains? node :val) node
        (or (nil? code)
            (boolean? code)
            (int? code)
            (float? code)
            (string? code)
            (keyword? code)) {:code code :val code}

        (symbol? code) (if (contains? bindings code)
                         {:code code :val (bindings code)}
                         {:code code})

        (and (list? code) (< (count code) 1))
        {:code code :error :no-fn}

        (list? code)
        (let [elems (mapv #(deval-node % bindings) code)
              all-final (reduce #(and %1 (contains? %2 :val)) true elems)]
          (if all-final
            {:code code :val (apply (:val (first elems)) (map :val (drop 1 elems)))}
            {:code (apply list elems)}))

        (coll? code) {:code (w/walk #(deval-node % bindings) identity code)}

        :else (println code)))

(defn deval
  #_(t/are [x y] (= x y)
      (deval '(+ a (+ b (+ c d))) {}) '(+ a (+ b (+ c d)))
      (deval '(+ a (+ b (+ c d))) {'d 1}) '(+ a (+ b (+ c 1)))
      (deval '(+ a (+ b (+ c d))) {'d 1 'c 2 '+ +}) '(+ a (+ b 3)))
  [code bindings]
  (node->code (deval-node (code->node code) bindings)))

(comment
  (def code '(+ a (+ b (+ c d))))
  (def node (code->node code))
  (deval code {'d 1 'c 2 '+ +})
  ;; => (+ a (+ b 3))
  )