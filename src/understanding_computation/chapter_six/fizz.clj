(ns understanding-computation.chapter-six.fizz)

(defn fizzbuzz []
  (letfn [(fizzbuzzify [n]
            (cond (= 0 (mod n 15)) "fizzbuzz"
                  (= 0 (mod n 5)) "buzz"
                  (= 0 (mod n 3)) "fizz"
                  :else n))]
    (->> (range 1 101)
         (map fizzbuzzify))))

(def ZERO    (fn [p] (fn [x] x)))
(def ONE     (fn [p] (fn [x] (p x))))
(def TWO     (fn [p] (fn [x] (p (p x)))))
(def THREE   (fn [p] (fn [x] (p (p (p x))))))
(def FIVE    (fn [p] (fn [x] (p (p (p (p (p x))))))))
(def FIFTEEN (fn [p] (fn [x] (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p x))))))))))))))))))
(def HUNDRED (fn [p] (fn [x] (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p (p x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;;; No macros allowed! :(

(def TRUE  (fn [x] (fn [y] x)))
(def FALSE (fn [x] (fn [y] y)))

(def IF (fn [b] b))

(def IS_ZERO (fn [n] ((n (fn [x] FALSE)) TRUE)))

(def PAIR  (fn [x] (fn [y] (fn [f] ((f x) y)))))
(def LEFT  (fn [p] (p (fn [x] (fn [y] x)))))
(def RIGHT (fn [p] (p (fn [x] (fn [y] y)))))

(def INCREMENT (fn [n] (fn [p] (fn [x] (p ((n p) x))))))

(def SLIDE (fn [p]
             ((PAIR
                (RIGHT p))
              (INCREMENT
               (RIGHT p)))))

(def DECREMENT (fn [n]
                 (LEFT
                  ((n SLIDE)
                   ((PAIR ZERO) ZERO)))))

(def ADD (fn [m]
           (fn [n]
             ((n INCREMENT) m))))
(def SUBTRACT (fn [m]
                (fn [n]
                  ((n DECREMENT) m))))
(def MULTIPLY (fn [m]
                (fn [n]
                  ((n (ADD m)) ZERO))))
(def POWER (fn [m]
             (fn [n]
               ((n (MULTIPLY m)) ONE))))

(def IS_LESS_OR_EQUAL (fn [m]
                        (fn [n]
                          (IS_ZERO ((SUBTRACT m) n)))))

(def Z (fn [f]
         ((fn [x]
             (f (fn [y]
                  ((x x) y))))
          (fn [x]
            (f (fn [y]
                 ((x x) y)))))))

(def MOD (Z (fn [f]
              (fn [m]
                (fn [n]
                  (((IF ((IS_LESS_OR_EQUAL n) m))
                    (fn [x] (((f ((SUBTRACT m) n)) n) x))) m))))))

(def EMPTY ((PAIR TRUE) TRUE))
(def UNSHIFT (fn [l]
               (fn [x]
                 ((PAIR FALSE) ((PAIR x) l)))))
(def IS_EMPTY LEFT)
(def FIRST (fn [l]
             (LEFT (RIGHT l))))
(def REST (fn [l]
            (RIGHT (RIGHT l))))

(def RANGE (Z (fn [f]
                (fn [m]
                  (fn [n]
                    (((IF ((IS_LESS_OR_EQUAL m) n))
                      (fn [x]
                        (((UNSHIFT ((f (INCREMENT m)) n)) m) x)))
                     EMPTY))))))

(def FOLD (Z (fn [f]
               (fn [l]
                 (fn [x]
                   (fn [g]
                     (((IF (IS_EMPTY l))
                       x)
                      (fn [y]
                        (((g (((f(REST l)) x) g)) (FIRST l)) y)))))))))

(def MAP (fn [k]
           (fn [f]
             (((FOLD k) EMPTY) (fn [l]
                                 (fn [x]
                                   ((UNSHIFT l) (f x))))))))

(def TEN ((MULTIPLY TWO) FIVE))
(def B TEN)
(def F (INCREMENT B))
(def I (INCREMENT F))
(def U (INCREMENT I))
(def ZED (INCREMENT U))

(def FIZZ ((UNSHIFT ((UNSHIFT ((UNSHIFT ((UNSHIFT EMPTY) ZED)) ZED)) I)) F))
(def BUZZ ((UNSHIFT ((UNSHIFT ((UNSHIFT ((UNSHIFT EMPTY) ZED)) ZED)) U)) B))
(def FIZZBUZZ ((UNSHIFT ((UNSHIFT ((UNSHIFT ((UNSHIFT BUZZ) ZED)) ZED)) I)) F))

(def DIV (Z (fn [f]
              (fn [m]
                (fn [n]
                  (((IF ((IS_LESS_OR_EQUAL n) m))
                    (fn [x]
                      ((INCREMENT ((f ((SUBTRACT m) n)) n)) x)))
                   ZERO))))))

(def PUSH (fn [l]
            (fn [x]
              (((FOLD l) ((UNSHIFT EMPTY) x)) UNSHIFT))))

(def TO_DIGITS (Z (fn [f]
                    (fn [n]
                      ((PUSH
                        (((IF
                           ((IS_LESS_OR_EQUAL n) (DECREMENT TEN)))
                          EMPTY)
                         (fn [x] ((f ((DIV n) TEN)) x))))
                       ((MOD n) TEN))))))

(defn to-integer [p]
  ((p inc) 0))

(defn to-boolean [p]
  (((IF p) true) false))

(defn to-array [a]
  (loop [a a ret []]
    (if (to-boolean (IS_EMPTY a))
      ret
      (recur (REST a) (conj ret (FIRST a))))))

(defn to-char [c]
  (nth "0123456789BFiuz" (to-integer c)))

(defn to-string [s]
  (apply str (map to-char (to-array s))))

(comment
  (map to-string
       (to-array
        ((MAP ((RANGE ONE) HUNDRED))
         (fn [n]
           (((IF (IS_ZERO ((MOD n) FIFTEEN)))
             FIZZBUZZ)
            (((IF (IS_ZERO ((MOD n) THREE)))
              FIZZ)
             (((IF (IS_ZERO ((MOD n) FIVE))) BUZZ) (TO_DIGITS n)))))))))
