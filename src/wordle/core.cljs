(ns wordle.core
  (:require [wordle.words :refer [words]]
            [wordle.solver :refer [check-words-difference]]))

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
    (set! (.-innerHTML app) "")
    (.appendChild app board)
    (reset! listener input-listener)
    (js/document.addEventListener
     "keydown"
     input-listener)))

(set! js/window.onload mount)


(comment



  (check-words-difference (list "y" "e" "t") (list "h" "e" "y")))