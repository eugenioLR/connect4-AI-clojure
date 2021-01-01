(ns connect4AI.game (:gen-class))
(require '[clojure.core.reducers :as r])

;======= CODE =========
(def SLOT-AMOUNT 4)

; slot {-1, 0, 1}
(defn empty-board
    ([height width]
        (empty-board height width 0 0 (vector) (vector)))
    ([height width i j board row]
        (cond
            (>= j height) board
            (>= i width) (empty-board height width 0 (+ j 1) (conj board row) (vector))
            :else (empty-board height width (+ i 1) j board (conj row -1)))))

(defn gen-legal-moves [board]
    (for [x (range (count (get board 0)))
          :when (= (get (get board 0) x) -1)]
        x))

(defn board-full? [board]
    (reduce #(and %1 %2) (for [i board] (reduce #(and %1 (not= %2 -1)) true i))))

(defn insert-piece
    ([board player x]
        (insert-piece board player x 0))
    ([board player x y]
        (cond
            (not= (get (get board y) x) -1) board
            (or (>= (+ y 1) (count board))
                (not= (get (get board (+ y 1)) x) -1))
                (assoc board y (assoc (get board y) x player))
            :else (insert-piece board player x (+ y 1)))))

(defn column-top
    ([board column]
        (column-top board column 0))
    ([board column i]
        (if (or (not= (get (get board i) column) -1)
                (>= i (count board)))
            i
            (column-top board column (+ i 1)))))

;move cursor to the start of a potencial line
(defn setup-check [board player coord step rev]
    (let [next (step coord) y (first next) x (last next)]
        (if (= (get (get board y) x) player)
            (setup-check board player next step rev)
            (rev coord))))

;start from the start of the potencial line and go step by step
;until either you reach the end or you get 4 in a row
(defn line-check
    ([board player coord step reverse]
        (line-check board player (setup-check board player coord reverse step) step reverse 1))
    ([board player coord step reverse count]
        (let [y (first coord) x (last coord)]
            (cond
                (>= count SLOT-AMOUNT) true
                (= (get (get board y) x) player)
                   (line-check board player (step coord) step reverse (+ count 1))
                :else false))))

;step functions
(defn step-up [coord] (list (- (first coord) 1) (last coord)))
(defn step-down [coord] (list (+ (first coord) 1) (last coord)))
(defn step-left [coord] (list (first coord) (- (last coord) 1)))
(defn step-right [coord] (list (first coord) (+ (last coord) 1)))

;line checking shorthand expressions
(defn vert-line-check [board player coord] (line-check board player coord step-up step-down))
(defn horiz-line-check [board player coord] (line-check board player coord step-left step-right))
(defn diag-line-check [board player coord] (line-check board player coord #(step-up (step-left %)) #(step-down (step-right %))))
(defn inv-diag-line-check [board player coord] (line-check board player coord #(step-up (step-right %)) #(step-down (step-left %))))

;checks if the player has completed any line
(defn game-ended? [board player coord]
    (or (vert-line-check board player coord)
        (horiz-line-check board player coord)
        (diag-line-check board player coord)
        (inv-diag-line-check board player coord)))
