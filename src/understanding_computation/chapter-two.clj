(ns understanding-computation.chapter-two)

(defprotocol Statement
  (reducable? [e]))

(defprotocol Reducable
  (reduce [e env]))

(defn- reduce-binary-expr-fn [self-constructor value-constructor operator]
  (fn [expression env]
    (cond
     (reducable? (:left expression)) (self-constructor
                                      (reduce (:left expression) env)
                                      (:right expression))
     (reducable? (:right expression)) (self-constructor
                                       (:left expression)
                                       (reduce (:right expression) env))
     :else (value-constructor (operator (:value (:left expression))
                                        (:value (:right expression)))))))

(defrecord SBoolean [value]
  Statement
  (reducable? [_] false))

(defrecord SNumber [value]
  Statement
  (reducable? [_] false))

(defrecord SAdd [left right]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((reduce-binary-expr-fn ->SAdd ->SNumber +) e env)))

(defrecord SMultiply [left right]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((reduce-binary-expr-fn ->SMultiply ->SNumber *) e env)))

(defrecord SLessThan [left right]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((reduce-binary-expr-fn ->SLessThan ->SBoolean <) e env)))

(defrecord SVariable [name]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [e env]
    ((:name e) env)))

(defrecord DoNothing []
  Statement
  (reducable? [_] false))

(defrecord Assign [name expression]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [assign env]
    (let [{:keys [name expression]} assign]
      (if (reducable? expression)
        {:statement (Assign. name (reduce expression env)) :env env}
        {:statement (DoNothing.) :env (assoc env name expression)}))))

(defrecord SMachine [statement env])

(defn step [machine]
  (let [{:keys [statement env]} (reduce
                                 (:statement machine)
                                 (:env machine))]
    (SMachine. statement env)))

(defn run [machine]
  (loop [m machine]
    (print (:statement m))
    (print ", ")
    (print (:env m))
    (print "\n")
    (if (reducable? (:statement m))
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

(defmethod clojure.core/print-method DoNothing [v writer]
  (.write writer "«do-nothing»"))

(defmethod clojure.core/print-method Assign [a writer]
  (.write writer (str "«" (:name a) " = "))
  (clojure.core/print-method (:expression a) writer)
  (.write writer "»"))
