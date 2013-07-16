(ns understanding-computation.chapter-three.dfa)

(defn rule [state character next-state]
  {:state state
   :character character
   :next-state next-state})

(defn rule-matching-fn [state character]
  (fn [rule]
    (= {:state state :character character}
       (select-keys rule [:state :character]))))

(defn next-state [rules state character]
  (let [matching-rule (first (filter (rule-matching-fn state character) rules))]
    (:next-state matching-rule)))

(defn acceptable? [string accept-states rules]
  (let [starting-state 1
        consume (fn [string state]
                  (if (empty? string) state
                      (recur (rest string) (next-state rules state (first string)))))
        final-state (consume string starting-state)]
    (some #{final-state} accept-states)))

(comment
  (def rules [(rule 1 \a 2)
              (rule 1 \b 1)
              (rule 2 \a 2)
              (rule 2 \b 3)
              (rule 3 \a 3)
              (rule 3 \b 3)])
  (acceptable? "a" [3] rules)
  (acceptable? "baa" [3] rules)
  (acceptable? "baba" [3] rules))
