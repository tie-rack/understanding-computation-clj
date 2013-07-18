(ns understanding-computation.chapter-three.nfa)

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

(defn acceptable? [string accept-states rules]
  (let [starting-state 1
        consume (fn [string states]
                  (if (empty? string) states
                      (recur (rest string) (next-states rules states (first string)))))
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
