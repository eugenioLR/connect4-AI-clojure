(ns connect4AI.game)

(require '[clojure.test :refer :all])

;======= CODE =========
(def SLOT-AMOUNT 4)
(def PLAYER-1 0)
(def PLAYER-2 1)

; slot {:player :filled}
(defn empty-board
    ([height width] (empty-board height width 0 0 (vector) (vector)))
    ([height width i j board row] (cond
                            (>= j height) board
                            (>= i width) (empty-board height width 0 (+ j 1) (conj board row) (vector))
                            :else (empty-board height width (+ i 1) j board (conj row {:player 0 :filled false})))))

(defn insert-piece
    ([board player x] (insert-piece board player x 0))
    ([board player x y] (cond
                            (get (get (get board y) x) :filled) board
                            (or (>= (+ y 1) (count board))
                                (get (get (get board (+ y 1)) x) :filled))
                                (assoc board y (assoc (get board y) x {:player player :filled true}))
                            :else (insert-piece board player x (+ y 1)))))

(defn setup-check [board player coord dir rev]
    (let [next (dir coord) y (first next) x (last next)]
        (if (and (= (get (get (get board y) x) :player) player)
                 (get (get (get board y) x) :filled))
            (setup-check board player next dir rev)
            (rev coord))))

(defn line-check
    ([board player coord step reverse] (line-check board player (setup-check board player coord reverse step) step reverse 1))
    ([board player coord step reverse count] (let [y (first coord) x (last coord)]
                                                (cond
                                                    (>= count SLOT-AMOUNT) true
                                                    (and (get (get (get board y) x) :filled)
                                                         (= (get (get (get board y) x) :player) player))
                                                         (line-check board player (step coord) step reverse (+ count 1))
                                                    :else false))))

(defn step-up [coord] (list (- (first coord) 1) (last coord)))
(defn step-down [coord] (list (+ (first coord) 1) (last coord)))
(defn step-left [coord] (list (first coord) (- (last coord) 1)))
(defn step-right [coord] (list (first coord) (+ (last coord) 1)))

(defn vert-line-check [board player coord] (line-check board player coord step-up step-down))
(defn horiz-line-check [board player coord] (line-check board player coord step-left step-right))
(defn diag-line-check [board player coord] (line-check board player coord #(step-up (step-left %)) #(step-down (step-right %))))
(defn inv-diag-line-check [board player coord] (line-check board player coord #(step-up (step-right %)) #(step-down (step-left %))))

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
    (is (= (get (first (first board)) :player) 0))
    (is (not (get (first (first board)) :filled))))

(deftest piece-insertion-test
    (def A {:player PLAYER-1 :filled true})
    (def B {:player PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
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
    (is (get (get (get (insert-piece test-board-0 0 0) 5) 0) :filled))
    (is (= (get (get (get (insert-piece test-board-0 1 0) 5) 0) :player) 1))
    (is (get (get (get (insert-piece test-board-1 2 0) 3) 0) :filled))
    (is (insert-piece test-board-2 0 0)))

(deftest line-checking-test
    (def A {:player PLAYER-1 :filled true})
    (def B {:player PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
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

(run-tests)
