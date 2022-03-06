(ns wordle.core
  (:require [wordle.words :refer [words]]
            [wordle.solver :refer [check-words-difference]]))

(def row-1 '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p"))
(def row-2 '("a" "s" "d" "f" "g" "h" "j" "k" "l"))
(def row-3 '("enter" "z" "x" "c" "v" "b" "n" "m" "del"))

(def keyboard-state (atom {}))
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
  (let [board-container (js/document.createElement "div")
        board (js/document.createElement "div")]
    (set! (.-className board-container) "board-container")
    (set! (.-className board) "board")
    (.appendChild board-container board)
    (doseq [_ (range n)]
      (let [cell (make-cell)]
        (swap! board-state conj cell)
        (.appendChild board cell)))

    board-container))

(defn get-letter [cell]
  (.-textContent cell))

(defn color-cells! [cells difference]
  (let [add-class (fn [el class] (.add (.-classList el) class))]
    (doseq [[idx cell] (map-indexed vector cells)]
      (condp = (nth difference idx)
        :exact (add-class cell "exact")
        :misplaced (add-class cell "misplaced")
        :miss (add-class cell "miss")))))

(defn color-keyboard-keys! [letters difference]
  (let [add-class (fn [el class]
                    (when-not (.contains (.-classList el) "exact")
                      (cond
                        (and (.contains (.-classList el) "misplaced") (= class "exact")) (.replace (.-classList el) "misplaced" "exact")
                        (and (not (.contains (.-classList el) "misplaced")) (= class "misplaced")) (.replace (.-classList el) "idle" "misplaced")
                        :else (.replace (.-classList el) "idle" class))))
        get-keyboard-key (fn [idx letters]
                           (get @keyboard-state (nth letters idx)))]
    (doseq [[idx difference] (map-indexed vector difference)]
      (condp = difference
        :exact (add-class (get-keyboard-key idx letters) "exact")
        :misplaced (add-class (get-keyboard-key idx letters) "misplaced")
        :miss (add-class (get-keyboard-key idx letters) "miss")))))

(defn check-solution [cells]
  (let [letters (mapv get-letter cells)
        difference (check-words-difference letters (vec @word-of-the-day))]
    (color-cells! cells difference)
    (color-keyboard-keys! letters difference)
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

(defn make-keyboard-key [key]
  (let [cell (js/document.createElement "div")
        input-listener (fn []
                         (user-input (.toLowerCase (cond
                                                     (= key "del") "backspace"
                                                     :else key))))]
    (set! (.-className cell) "keyboard-key idle")
    (set! (.-textContent cell) key)
    (.addEventListener cell
                       "click"
                       input-listener)
    (swap! keyboard-state into [[key cell]])
    cell))

(defn make-keyboard-row [row]
  (let [row-element (js/document.createElement "div")]
    (set! (.-className row-element) "row")
    (doseq [key row]
      (let [key-element (make-keyboard-key key)]
        (.appendChild row-element key-element)))
    row-element))

(defn make-keyboard [& rows]
  (let [keyboard-element (js/document.createElement "div")]
    (set! (.-className keyboard-element) "keyboard")
    (doseq [row rows]
      (let [row-element (make-keyboard-row row)]
        (.appendChild keyboard-element row-element)))
    keyboard-element))

(defn ^:dev/before-load unmount []
  (js/document.removeEventListener "keydown" @listener)
  (let [app (js/document.getElementById "app")]
    (set! (.-innerHTML app) "")))

(defn mount []
  (let [app (js/document.getElementById "app")
        board (make-board 30)
        keyboard (make-keyboard row-1 row-2 row-3)
        input-listener (fn [e]
                         (user-input (.toLowerCase (.-key e))))]
    (set! (.-innerHTML app) "")
    (.appendChild app board)
    (.appendChild app keyboard)
    (reset! listener input-listener)
    (js/document.addEventListener
     "keydown"
     input-listener)))

;(mount)
(set! js/window.onload mount)


(comment



  (check-words-difference (list "y" "e" "t") (list "h" "e" "y")))