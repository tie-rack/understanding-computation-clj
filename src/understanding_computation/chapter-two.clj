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

(defrecord SDoNothing []
  Statement
  (reducable? [_] false))

(defrecord SAssign [name expression]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [assign env]
    (let [{:keys [name expression]} assign]
      (if (reducable? expression)
        {:statement (SAssign. name (reduce expression env)) :env env}
        {:statement (SDoNothing.) :env (assoc env name expression)}))))

(defrecord SIf [condition consequence alternative]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [sif env]
    (let [{:keys [condition consequence alternative]} sif]
      (cond
       (reducable? condition) {:statement
                               (SIf. (reduce condition env)
                                     consequence
                                     alternative)
                               :env env}
       (= condition (SBoolean. true)) {:statement consequence :env env}
       (= condition (SBoolean. false)) {:statement alternative :env env}))))

(defrecord SSequence [first second]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [s env]
    (if (= (:first s) (SDoNothing.))
      {:statement (:second s)
       :env env}
      (let [{:keys [statement env]} (reduce (:first s) env)]
        {:statement (SSequence. statement (:second s))
         :env env}))))

(defrecord SWhile [condition body]
  Statement
  (reducable? [_] true)
  Reducable
  (reduce [swhile env]
    {:statement (SIf. (:condition swhile)
                      (SSequence. (:body swhile) swhile)
                      (SDoNothing.))
     :env env}))

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

(defmethod clojure.core/print-method SDoNothing [v writer]
  (.write writer "«do-nothing»"))

(defmethod clojure.core/print-method SAssign [a writer]
  (.write writer (str "«" (:name a) " = "))
  (clojure.core/print-method (:expression a) writer)
  (.write writer "»"))

(defmethod clojure.core/print-method SIf [sif writer]
  (.write writer "if (")
  (clojure.core/print-method (:condition sif) writer)
  (.write writer ") { ")
  (clojure.core/print-method (:consequence sif) writer)
  (.write writer " } else { ")
  (clojure.core/print-method (:alternative sif) writer)
  (.write writer " }"))

(defmethod clojure.core/print-method SSequence [s writer]
  (clojure.core/print-method (:first s) writer)
  (.write writer "; ")
  (clojure.core/print-method (:second s) writer))

(defmethod clojure.core/print-method SWhile [swhile writer]
  (.write writer "while (")
  (clojure.core/print-method (:condition swhile) writer)
  (.write writer ") { ")
  (clojure.core/print-method (:body swhile) writer)
  (.write writer " }"))
