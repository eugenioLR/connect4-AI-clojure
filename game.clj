(ns game)

(require '[clojure.test :refer :all])
(require '[clojure.core.reducers :as r])

;======= CODE =========
(def SLOT-AMOUNT 4)
(def PLAYER-1 0)
(def PLAYER-2 1)

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

;======= TESTS =========
(deftest board-generation-test
    (def board (empty-board 6 7))
    (is (= (count board) 6))
    (is (= (count (first board)) 7))
    (is (= (first (first board)) -1))
    (is (= (first (first board)) -1)))

(deftest legal-moves-tests
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board-0 (game/empty-board 6 7))
    (def test-board-1 [[A N N A N B N]
                       [A N B A N B A]
                       [B A B A N B A]
                       [B A A A N A B]
                       [A B A B N B A]
                       [A B A B N A A]])
    (is (= (gen-legal-moves test-board-0) (range 7)))
    (is (= (gen-legal-moves test-board-1) '(1 2 4 6))))

(deftest board-full-tests
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board-1 (empty-board 6 7))
    (def test-board-2 [[A A A A A A A]
                       [A A A A A A A]
                       [A A A A A A A]
                       [A A A A A A A]
                       [A A A A A A A]
                       [A A A A A A A]])
    (def test-board-3 [[A A A A A A A]
                       [A A A A A N A]
                       [A A A A A A A]
                       [A A A A A A A]
                       [A A A A A A A]
                       [A A A A A A A]])
    (is (not (board-full? test-board-1)))
    (is (board-full? test-board-2))
    (is (not (board-full? test-board-3))))

(deftest piece-insertion-test
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board-0 (empty-board 6 7))
    (def test-board-1 [[N N N N N N]
                       [N N N N N N]
                       [A N N N N N]
                       [A A N N N N]
                       [A A A N N N]
                       [A A A A N N]])
    (def test-board-2 [[A N N N N N]
                       [A N N N N N]
                       [A N N N N N]
                       [B B N N N N]
                       [A B A N N N]
                       [A A B A N N]])
    (is (not= (get (get (insert-piece test-board-0 0 0) 5) 0) -1))
    (is (= (get (get (insert-piece test-board-0 1 0) 5) 0) 1))
    (is (not= (get (get (insert-piece test-board-1 2 0) 3) 0) -1))
    (is (insert-piece test-board-2 0 0)))

(deftest line-checking-test
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board-1 [[N N N N N N]
                       [N N N N N N]
                       [A N N N A N]
                       [A A N A N N]
                       [A A A N N N]
                       [A A A A N N]])

    (def test-board-2 [[A N N N N N]
                       [A N N N N A]
                       [B N N N B N]
                       [A A N A N N]
                       [A B A N N N]
                       [A A B A N N]])

    (def test-board-3 [[N N N N N N]
                       [N N N N N N]
                       [N N N N N N]
                       [N N N N N N]
                       [A N N N N N]
                       [A A A B N N]])
    ;vertical lines
    (is (vert-line-check test-board-1 PLAYER-1 (list 2 0)))
    (is (not (vert-line-check test-board-2 PLAYER-1 (list 2 0))))
    (is (not (vert-line-check test-board-3 PLAYER-2 (list 5 0))))
    (is (not (vert-line-check test-board-3 PLAYER-2 (list 2 0))))
    ;horizontal lines
    (is (horiz-line-check test-board-1 PLAYER-1 (list 5 3)))
    (is (not (horiz-line-check test-board-2 PLAYER-1 (list 5 1))))
    (is (not (horiz-line-check test-board-3 PLAYER-2 (list 5 2))))
    (is (not (horiz-line-check test-board-3 PLAYER-2 (list 2 2))))
    ;regular diagonal lines
    (is (diag-line-check test-board-1 PLAYER-1 (list 4 2)))
    (is (not (diag-line-check test-board-2 PLAYER-1 (list 4 2))))
    (is (not (diag-line-check test-board-3 PLAYER-2 (list 5 2))))
    (is (not (diag-line-check test-board-3 PLAYER-2 (list 2 2))))
    ;inverse diagonal lines
    (is (inv-diag-line-check test-board-1 PLAYER-1 (list 4 2)))
    (is (not (inv-diag-line-check test-board-2 PLAYER-1 (list 4 2))))
    (is (not (inv-diag-line-check test-board-3 PLAYER-2 (list 5 2))))
    (is (not (inv-diag-line-check test-board-3 PLAYER-2 (list 2 2)))))

(deftest win-condition-test
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board-1 [[N N N N N N]
                       [N N N N N N]
                       [A N N N A N]
                       [A A N A N N]
                       [A A A N N N]
                       [A A A A N N]])

    (def test-board-2 [[A N N N N N]
                       [A N N N N A]
                       [B N N N B N]
                       [A A N A N N]
                       [A B A N N N]
                       [A A B A N N]])
    (is (game-ended? test-board-1 PLAYER-1 '(4 2)))
    (is (game-ended? test-board-1 PLAYER-1 '(5 1)))
    (is (game-ended? test-board-1 PLAYER-1 '(5 0)))
    (is (not (game-ended? test-board-2 PLAYER-1 '(4 2))))
    (is (not (game-ended? test-board-2 PLAYER-1 '(5 1))))
    (is (not (game-ended? test-board-2 PLAYER-1 '(5 0)))))

;(run-tests)
