(ns understanding-computation.chapter-two)

(defprotocol SExpression
  (reducable? [e]))

(defprotocol Reducable
  (reduce [e env]))

(defn- reduce-binary-expr-fn [self-constructor value-constructor operator]
  (fn [expression env]
    (cond
     (reducable? (:left expression)) (self-constructor (reduce (:left expression) env) (:right expression))
     (reducable? (:right expression)) (self-constructor (:left expression) (reduce (:right expression) env))
     :else (value-constructor (operator (:value (:left expression)) (:value (:right expression)))))))

(defrecord SBoolean [value]
  SExpression
  (reducable? [_] false))

(defrecord SNumber [value]
  SExpression
  (reducable? [_] false))

(defrecord SAdd [left right]
  SExpression
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((reduce-binary-expr-fn ->SAdd ->SNumber +) e env)))

(defrecord SMultiply [left right]
  SExpression
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((reduce-binary-expr-fn ->SMultiply ->SNumber *) e env)))

(defrecord SLessThan [left right]
  SExpression
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((reduce-binary-expr-fn ->SLessThan ->SBoolean <) e env)))

(defrecord SVariable [name]
  SExpression
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((:name e) env)))

(defrecord SMachine [expression env])

(defn step [machine]
  (SMachine. (reduce (:expression machine) (:env machine)) (:env machine)))

(defn run [machine]
  (loop [m machine]
    (print (:expression m))
    (print "\n")
    (if (reducable? (:expression m))
      (recur (step m)))))

;;; Printing
(defn- print-binary-epxr [operator]
  (fn [expression writer]
    (clojure.core/print-method (:left expression) writer)
    (.write writer (str " " operator " "))
    (clojure.core/print-method (:right expression) writer)))

(defmethod clojure.core/print-method SBoolean [b writer]
  (.write writer (str "«" (:value b) "»")))

(defmethod clojure.core/print-method SNumber [n writer]
  (.write writer (str "«" (:value n) "»")))

(defmethod clojure.core/print-method SAdd [expression writer]
  ((print-binary-epxr "+") expression writer))

(defmethod clojure.core/print-method SMultiply [expression writer]
  ((print-binary-epxr "*") expression writer))

(defmethod clojure.core/print-method SLessThan [expression writer]
  ((print-binary-epxr "<") expression writer))

(defmethod clojure.core/print-method SVariable [v writer]
  (.write writer (str "«" (:name v) "»")))
