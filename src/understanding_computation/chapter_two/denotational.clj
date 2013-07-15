(ns understanding-computation.chapter-two.denotational)

(defprotocol Clojureable
  (to-clojure [clojureable]))

(defn execute [clojureable env]
  ((eval (to-clojure clojureable)) env))

(defrecord SNumber [value]
  Clojureable
  (to-clojure [n]
    `(fn [env#] (:value ~n))))

(defrecord SVariable [name]
  Clojureable
  (to-clojure [v]
    `(fn [env#] (env# (:name ~v)))))

(defrecord SAdd [l r]
  Clojureable
  (to-clojure [a]
    `(fn [env#]
       (+ (execute (:l ~a) env#)
          (execute (:r ~a) env#)))))

;; Printing
(defn- print-binary-epxr [operator]
  (fn [expression writer]
    (clojure.core/print-method (:l expression) writer)
    (.write writer (str " " operator " "))
    (clojure.core/print-method (:r expression) writer)))

;; (defmethod clojure.core/print-method SBoolean [b writer]
;;   (.write writer (str "«" (:value b) "»")))

(defmethod clojure.core/print-method SNumber [n writer]
  (.write writer (str "«" (:value n) "»")))

(defmethod clojure.core/print-method SAdd [expression writer]
  ((print-binary-epxr "+") expression writer))

;; (defmethod clojure.core/print-method SMultiply [expression writer]
;;   ((print-binary-epxr "*") expression writer))

;; (defmethod clojure.core/print-method SLessThan [expression writer]
;;   ((print-binary-epxr "<") expression writer))

(defmethod clojure.core/print-method SVariable [v writer]
  (.write writer (str "«" (:name v) "»")))

;; (defmethod clojure.core/print-method SDoNothing [v writer]
;;   (.write writer "«do-nothing»"))

;; (defmethod clojure.core/print-method SAssign [a writer]
;;   (.write writer (str "«" (:name a) " = "))
;;   (clojure.core/print-method (:expr a) writer)
;;   (.write writer "»"))

;; (defmethod clojure.core/print-method SIf [sif writer]
;;   (.write writer "if (")
;;   (clojure.core/print-method (:condition sif) writer)
;;   (.write writer ") { ")
;;   (clojure.core/print-method (:consequence sif) writer)
;;   (.write writer " } else { ")
;;   (clojure.core/print-method (:alternative sif) writer)
;;   (.write writer " }"))

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
