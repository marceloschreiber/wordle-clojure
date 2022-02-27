(ns wordle.core
  (:require [wordle.words :refer [words]]))

(def board-state (atom []))
(def counter (atom 0))
(def attempt (atom 0))
(def word-of-the-day (atom (rand-nth (seq words))))

(println @word-of-the-day)

(defn write-letter [cell letter]
  (set! (.-textContent cell) letter))

(defn make-cell []
  (let [cell (js/document.createElement "div")]
    (set! (.-className cell) "cell")
    cell))

(defn make-board [n]
  (let [board (js/document.createElement "div")]
    (set! (.-className board) "board")
    (doseq [_ (range n)]
      (let [cell (make-cell)]
        (swap! board-state conj cell)
        (.appendChild board cell)))
    board))

(defn get-letter [cell]
  (.-textContent cell))

(defn color-cells! [cells difference]
  (let [add-class (fn [el class] (.add (.-classList el) class))]
    (doseq [[idx cell] (map-indexed vector cells)]
      (condp = (nth difference idx)
        :exact (add-class cell "exact")
        :misplaced (add-class cell "misplaced")
        :miss (add-class cell "miss")))))

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
  [guess solution]
  (let [result (atom (letters-in-right-place guess solution))]
    (set-misplaced-letters! guess solution result)))

(defn check-solution [cells]
  (let [difference (check-words-difference (mapv get-letter cells) (vec @word-of-the-day))]
    (color-cells! cells difference)
    (every? #(= :exact %) difference)))

(defonce listener (atom nil))

(defn user-input [key]
  (let [start (* 5 @attempt)
        end (* 5 (inc @attempt))]
    (cond
      (and (re-matches #"[a-z]" key)
           (< @counter end)) (do
                               (write-letter (nth @board-state @counter) key)
                               (swap! counter inc))
      (and (= key "backspace")
           (> @counter start)) (do (swap! counter dec)
                                   (write-letter (nth @board-state @counter) ""))
      (and (= key "enter")
           (zero? (mod @counter 5))) (when-not (check-solution (subvec @board-state start end))
                                       (if (= 5 @attempt)
                                         (js/document.removeEventListener "keydown" @listener)
                                         (swap! attempt inc))))))

(defn ^:dev/before-load unmount []
  (js/document.removeEventListener "keydown" @listener)
  (let [app (js/document.getElementById "app")]
    (set! (.-innerHTML app) "")))

(defn mount []
  (let [app (js/document.getElementById "app")
        board (make-board 30)
        input-listener (fn [e]
                         (user-input (.toLowerCase (.-key e))))]
    (.appendChild app board)
    (reset! listener input-listener)
    (js/document.addEventListener
     "keydown"
     input-listener)))

(mount)


(comment



  (check-words-difference (list "y" "e" "t") (list "h" "e" "y")))