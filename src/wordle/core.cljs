(ns wordle.core)

(def board-state (atom []))
(def counter (atom 0))
(def attempt (atom 0))
(def word-of-the-day (atom "hello"))

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

(defn color-cell [idx cell]
  (let [letter (get-letter cell)
        add-class (fn [el class] (.add (.-classList el) class))]
    (cond
      (= letter (nth @word-of-the-day idx)) (add-class cell "right")
      (contains? (set @word-of-the-day) letter) (add-class cell "missplaced")
      :else (add-class cell "wrong"))))

(defn check-solution [cells]
  (doseq [[idx cell] (map-indexed vector cells)]
    (color-cell idx cell))
  (= (mapv get-letter cells)
     (vec @word-of-the-day)))

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
           (zero? (mod @counter 5))) (do
                                       (when (check-solution (subvec @board-state start end))
                                         (js/alert "You won"))
                                       (swap! attempt inc)))))

(defonce listener (atom nil))

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