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

(defn to-integer [p]
  ((p inc) 0))

(defn to-boolean [p]
  (((IF p) true) false))
