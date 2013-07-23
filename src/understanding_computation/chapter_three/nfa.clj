(ns understanding-computation.chapter-three.nfa
  (require [clojure.set :as set]))

(defn rule [state character next-state]
  {:state state
   :character character
   :next-state next-state})

(defn rule-matching-fn [states character]
  (fn [rule]
    (and (= character (:character rule))
         ((set states) (:state rule)))))

(defn next-states [rules states character]
  (let [matching-rules (filter (rule-matching-fn states character) rules)]
    (set (map :next-state matching-rules))))

(defn follow-free-states [rules states]
  (let [more-states (next-states rules states nil)]
    (if (set/subset? more-states states)
      states
      (recur rules (set/union states more-states)))))

(defn consume-fn [rules]
  (fn [string states]
    (if (empty? string)
      states
      (recur (rest string)
             (next-states rules
                          (follow-free-states rules states)
                          (first string))))))

(defn acceptable? [string accept-states rules]
  (let [starting-state 1
        consume (consume-fn rules)
        final-states (consume string #{starting-state})]
    (boolean (some final-states accept-states))))

(comment
  (def rules [(rule 1 \a 1)
              (rule 1 \b 1)
              (rule 1 \b 2)
              (rule 2 \a 3)
              (rule 2 \b 3)
              (rule 3 \a 4)
              (rule 3 \b 4)])
  (acceptable? "bbabb" '(4) rules)
  (acceptable? "aabaaa" '(4) rules)
  (acceptable? "bb" '(4) rules)
  (acceptable? "bbb" '(4) rules))

(comment
  (def rules [(rule 1 nil 2)
              (rule 1 nil 4)
              (rule 2 \a 3)
              (rule 3 \a 2)
              (rule 4 \a 5)
              (rule 5 \a 6)
              (rule 6 \a 4)])
  (acceptable? "aa" '(2 4) rules)
  (acceptable? "aaa" '(2 4) rules)
  (acceptable? "aaaaa" '(2 4) rules)
  (acceptable? "aaaaaa" '(2 4) rules))
