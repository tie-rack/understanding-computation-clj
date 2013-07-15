(ns understanding-computation.chapter-two.denotational
  (:require [instaparse.core :as insta]))

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

(defn binary-op-to-clojure [binary-form op]
  `(fn [env#]
     (~op (execute (:l ~binary-form) env#)
          (execute (:r ~binary-form) env#))))

(defrecord SAdd [l r]
  Clojureable
  (to-clojure [a]
    (binary-op-to-clojure a +)))

(defrecord SMultiply [l r]
  Clojureable
  (to-clojure [m]
    (binary-op-to-clojure m *)))

(defrecord SLessThan [l r]
  Clojureable
  (to-clojure [lt]
    (binary-op-to-clojure lt <)))

(defrecord SAssign [name expr]
  Clojureable
  (to-clojure [a]
    `(fn [env#]
       (assoc env# (:name ~a) (execute (:expr ~a) env#)))))

(defrecord SDoNothing []
  Clojureable
  (to-clojure [_]
    identity))

(defrecord SIf [condition consequence alternative]
  Clojureable
  (to-clojure [sif]
    `(fn [env#]
       (if (execute (:condition ~sif) env#)
         (execute (:consequence ~sif) env#)
         (execute (:alternative ~sif) env#)))))

(defrecord SSequence [first second]
  Clojureable
  (to-clojure [sseq]
    `(fn [env#]
       (execute (:second ~sseq) (execute (:first ~sseq) env#)))))

(defrecord SWhile [condition body]
  Clojureable
  (to-clojure [swhile]
    `(fn [env#]
       (if (execute (:condition ~swhile) env#)
         (recur (execute (:body ~swhile) env#))
         (execute (SDoNothing.) env#)))))

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

(def parser
  (insta/parser
"program = statement
statement = assign | if | while
while = <'while('> expr <') {'> statement <'}'>
assign = name <'='> expr
number = #'[0-9]+'
name = #'[a-z]+'
if = <'if('> expr <') then {'> statement <'} else {'> statement <'}'>
expr = lt | add | mult
add = term <'+'> term
mult = term <'*'> term
lt = term <'<'> term
term = vname | number
vname = #'[a-z]+'
"))

(def transforms
  {:number #(SNumber. (read-string %))
   :name keyword
   :assign #(SAssign. % %2)
   :statement identity
   :if #(SIf. % %2 %3)
   :while #(SWhile. % %2)
   :term identity
   :lt #(SLessThan. % %2)
   :add #(SAdd. % %2)
   :mult #(SMultiply. % %2)
   :expr identity
   :program identity
   :vname #(SVariable. (keyword %))})

(defn parse-and-execute [program env]
  (let [ast (insta/transform transforms (parser program))]
    (execute ast env)))

(comment
  (parse-and-execute "if(x<3) then {y=1} else {y=2}" {:x 4})
  (parse-and-execute "while(x<5) {x=x*3}" {:x 1}))
