(ns understanding-computation.chapter-two.big-step)

(defprotocol Evalable
  (evaluate [evalable enviroment]))

(defn- eval-binary-expr [return-constructor operator]
  (fn [expr env]
    (return-constructor
     (operator
      (:value (evaluate (:l expr) env))
      (:value (evaluate (:r expr) env))))))

(defrecord SNumber [value]
  Evalable
  (evaluate [n _]
    n))

(defrecord SBoolean [value]
  Evalable
  (evaluate [b _]
    b))

(defrecord SVariable [name]
  Evalable
  (evaluate [v env]
    (env (:name v))))

(defrecord SAdd [l r]
  Evalable
  (evaluate [expr env]
    ((eval-binary-expr ->SNumber +) expr env)))

(defrecord SMultiply [l r]
  Evalable
  (evaluate [expr env]
    ((eval-binary-expr ->SNumber *) expr env)))

(defrecord SLessThan [l r]
  Evalable
  (evaluate [expr env]
    ((eval-binary-expr ->SBoolean <) expr env)))

(defrecord SAssign [name expr]
  Evalable
  (evaluate [sassign env]
    (assoc env (:name sassign) (evaluate (:expr sassign) env))))

(defrecord SDoNothing []
  Evalable
  (evaluate [_ env]
    env))

(defrecord SIf [condition consequence alternative]
  Evalable
  (evaluate [sif env]
    (let [{:keys [condition consequence alternative]} sif
          test (evaluate condition env)]
      (cond
       (= (SBoolean. true) test) (evaluate consequence env)
       (= (SBoolean. false) test) (evaluate alternative env)))))

;;; Printing
(defn- print-binary-epxr [operator]
  (fn [expression writer]
    (clojure.core/print-method (:l expression) writer)
    (.write writer (str " " operator " "))
    (clojure.core/print-method (:r expression) writer)))

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

;; (defmethod clojure.core/print-method SDoNothing [v writer]
;;   (.write writer "«do-nothing»"))

(defmethod clojure.core/print-method SAssign [a writer]
  (.write writer (str "«" (:name a) " = "))
  (clojure.core/print-method (:expr a) writer)
  (.write writer "»"))

(defmethod clojure.core/print-method SIf [sif writer]
  (.write writer "if (")
  (clojure.core/print-method (:condition sif) writer)
  (.write writer ") { ")
  (clojure.core/print-method (:consequence sif) writer)
  (.write writer " } else { ")
  (clojure.core/print-method (:alternative sif) writer)
  (.write writer " }"))

;; (defmethod clojure.core/print-method SSequence [s writer]
;;   (clojure.core/print-method (:first s) writer)
;;   (.write writer "; ")
;;   (clojure.core/print-method (:second s) writer))

;; (defmethod clojure.core/print-method SWhile [swhile writer]
;;   (.write writer "while (")
;;   (clojure.core/print-method (:condition swhile) writer)
;;   (.write writer ") { ")
;;   (clojure.core/print-method (:body swhile) writer)
;;   (.write writer " }"))
