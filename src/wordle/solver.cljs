(ns wordle.solver
  (:require [wordle.words :refer [words]]))

(defn letters-in-right-place
  "Returns a vector containing :miss or :exact for each letter position"
  [guess solution]
  (->> (map-indexed (fn [idx _]
                      (cond
                        (= (nth guess idx) (nth solution idx)) :exact
                        :else :miss))
                    guess)
       (apply vector)))

(defn set-misplaced-letters!
  "Return a vector containing :misplaced for the letters that are misplaced"
  [guess solution result]
  (let [guess-atom (atom @result)]
    (doseq [i (range (count @result))]
      (when-not (= :exact (nth @result i))
        (doseq [j (range (count solution))]
          (when (and (not= :misplaced (nth @result j)) (not= :exact (nth @result j)) (not= :misplaced (nth @guess-atom i)) (= (nth guess i) (nth solution j)))
            (swap! guess-atom assoc i :misplaced)
            (swap! result assoc j :misplaced)))))
    @guess-atom))

(defn check-words-difference
  "Returns a vector with keys such as :exact :misplaced :miss on each position of the word"
  [guess solution]
  (let [result (atom (letters-in-right-place guess solution))]
    (set-misplaced-letters! guess solution result)))

(defn word-match-difference?
  [guess word difference]
  (let [letter-matched (atom (vec (repeatedly (count word) (fn [] :not-matched))))
        is-valid? (atom true)]
    (doseq [[idx status] (map-indexed vector difference)]
      (condp = status
        :exact (if (and @is-valid? (= (nth guess idx) (nth word idx)))
                 (do
                   (swap! is-valid? #(and % (= (nth guess idx) (nth word idx))))
                   (swap! letter-matched assoc idx :matched))
                 (reset! is-valid? false))
        :misplaced (when @is-valid?
                     (doseq [[jdx status] (map-indexed vector difference)]
                       (when (and (not= :matched (nth @letter-matched jdx)) (= (nth guess idx) (nth word jdx)))
                         (swap! letter-matched assoc jdx :matched))))
        :miss (when @is-valid?
                (doseq [[jdx status] (map-indexed vector difference)]
                  (when (= (nth guess idx) (nth word jdx))
                    (reset! is-valid? false))))
        :else))
    (and @is-valid? (= (count (filter (fn [item] (= item :matched)) @letter-matched))
                       (count (filter (fn [item] (or (= :misplaced item) (= :exact item))) difference))))))


(word-match-difference? (seq "yucko") (seq "lippy") (check-words-difference (seq "yucko") (seq "lippy")))


(defn filter-possible-words
  [difference guess possible-words]
  (-> (filter (fn [word]
                (let [word-seq (seq word)]
                  (word-match-difference? guess word-seq difference)))
              possible-words)))

(filter-possible-words (check-words-difference (seq "yucko") (seq "viviv")) (seq "yucko") #{"yucko" "viviv" "smowt" "twoer" "bayes" "lippy" "naira" "iftar" "lemel" "posse"})

(defn calculate-word-entropy
  [guess solution possible-words]
  (-> (check-words-difference (seq guess) (seq solution))
      (filter-possible-words (seq guess) possible-words)
      (count)
      (/ (count possible-words))))

(calculate-word-entropy "smowt" "posse" #{"yucko" "viviv" "smowt" "twoer" "bayes" "lippy" "naira" "iftar" "lemel" "posse"})
  ; Cai fora: yucko smowt twoer bayes posse lippy
  ; Continua: viviv  naira iftar lemel

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn entropy-by-probability
  [probability]
  (if (zero? probability)
    0
    (* probability (log2 (/ 1 probability)))))

(entropy-by-probability 0)

(defn calculate-total-entropy
  [guess possible-words]
  (reduce +
          (map (fn [solution]
                 (let [word-entropy (calculate-word-entropy guess solution possible-words)]
                   (entropy-by-probability word-entropy)))
               possible-words)))

(comment
  ;; Checking logic
  (def word-set #{"bayes" "lemel" "iftar" "varia" "naira"})
  (def guess "bayes")

  (calculate-word-entropy "bayes" "naira" word-set)

  (def few-words (take-nth 5 words))
  (->> (map (fn [guess]
              {:guess guess
               :entropy (calculate-total-entropy guess few-words)})
            few-words)
       (sort-by :entropy)
       (reverse)))

