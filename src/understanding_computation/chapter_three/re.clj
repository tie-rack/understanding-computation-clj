(ns understanding-computation.chapter-three.re)

(defprotocol Pattern
  (precidence [p])
  (string-representation [p]))

(defn bracket [outer inner]
  (if (< (precidence inner) (precidence outer))
    (str "(" (string-representation inner) ")")
    (string-representation inner)))

(defrecord Empty []
  Pattern
  (precidence [_] 3)
  (string-representation [_] ""))

(defrecord Literal [character]
  Pattern
  (precidence [_] 3)
  (string-representation [p] (:character p)))

(defrecord Concatenate [left right]
  Pattern
  (precidence [_] 1)
  (string-representation [p]
    (str (bracket p (:left p))
         (bracket p (:right p)))))

(defrecord Choose [left right]
  Pattern
  (precidence [_] 0)
  (string-representation [p]
    (str (bracket p (:left p))
         "|"
         (bracket p (:right p)))))

(defrecord Repeat [pattern]
  Pattern
  (precidence [_] 2)
  (string-representation [p]
    (str (bracket p (:pattern p)) "*")))
