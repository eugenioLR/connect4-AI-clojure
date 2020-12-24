(ns connect4AI.game)

(require '[clojure.test :refer :all])

;======= CODE =========
(def SLOT-AMOUNT 4)
(def PLAYER-1 0)
(def PLAYER-2 1)

; slot {:player :filled}
(defn empty-board
    ([size] (empty-board size 0 0 (vector) (vector)))
    ([size i j board row] (cond
                            (>= j size) board
                            (>= i size) (empty-board size 0 (+ j 1) (conj board row) (vector))
                            :else (empty-board size (+ i 1) j board (conj row {:player 0 :filled false})))))

(defn insert-piece
    ([board player x] (insert-piece board player x 0))
    ([board player x y] (cond
                            (get (get (get board y) x) :filled) board
                            (or (>= (+ y 1) (count board))
                                (get (get (get board (+ y 1)) x) :filled))
                                (assoc board y (assoc (get board y) x {:player player :filled true}))
                            :else (insert-piece board player x (+ y 1)))))


;vertical line check
(defn vert-line-check
    ([board player y x] (vert-line-check board player y x 1))
    ([board player y x count] (cond
                                (>= count SLOT-AMOUNT) true
                                (and (get (get (get board y) x) :filled)
                                     (= (get (get (get board y) x) :player) player)) ;no need to check y < 0, nil => false
                                     (vert-line-check board player (+ y 1) x (+ count 1))
                                :else false)))

;horizontal line check
(defn set-check-left [board player y x]
    (if (and (= (get (get (get board y) (- x 1)) :player) player)
             (get (get (get board y) (- x 1)) :filled))
        (set-check-left board player y (- x 1))
        x))

(defn horiz-line-check
    ([board player y x] (horiz-line-check board player y (set-check-left board player y x) 1))
    ([board player y x count] (cond
                                (>= count SLOT-AMOUNT) true
                                (and (get (get (get board y) x) :filled)
                                     (= (get (get (get board y) x) :player) player))
                                     (horiz-line-check board player y (+ x 1) (+ count 1))
                                :else false)))

;regular diagonal check
(defn set-check-diag [board player y x]
    (if (and (= (get (get (get board (- y 1)) (- x 1)) :player) player)
             (get (get (get board (- y 1)) (- x 1)) :filled))
        (set-check-diag board player (- y 1) (- x 1))
        (list y x)))

(defn diag-line-check
    ([board player y x] (let [check-start (set-check-diag board player y x)]
                             (diag-line-check board player (first check-start) (last check-start) 0)))
    ([board player y x count] (cond
                                (>= count SLOT-AMOUNT) true
                                (and (get (get (get board y) x) :filled)
                                     (= (get (get (get board y) x) :player) player))
                                     (diag-line-check board player (+ y 1) (+ x 1) (+ count 1))
                                :else false)))

;inverse diagonal check
(defn set-check-inv-diag [board player y x]
    (if (and (= (get (get (get board (- y 1)) (+ x 1)) :player) player)
             (get (get (get board (- y 1)) (+ x 1)) :filled))
        (set-check-inv-diag board player (- y 1) (+ x 1))
        (list y x)))

(defn inv-diag-line-check
    ([board player y x] (let [check-start (set-check-inv-diag board player y x)]
                             (inv-diag-line-check board player (first check-start) (last check-start) 0)))
    ([board player y x count] (cond
                                (>= count SLOT-AMOUNT) true
                                (and (get (get (get board y) x) :filled)
                                     (= (get (get (get board y) x) :player) player))
                                     (inv-diag-line-check board player (+ y 1) (- x 1) (+ count 1))
                                :else false)))

(defn game-ended? [board player y x]
    (or (vert-line-check board player y x)
        (horiz-line-check board player y x)
        (diag-line-check board player y x)
        (inv-diag-line-check board player y x)))

;======= TESTS =========

(deftest board-generation
    (def board (empty-board 6))
    (is (= (count board) 6))
    (is (= (count (first board)) 6))
    (is (= (get (first (first board)) :player) 0))
    (is (not (get (first (first board)) :filled))))

(deftest piece-insertion
    (def A {:player PLAYER-1 :filled true})
    (def B {:player PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
    (def test-board-0 (empty-board 6))
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

(deftest line-check-setups
    (def A {:player PLAYER-1 :filled true})
    (def B {:player PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
    (def test-board-1 [[N N N N N N]
                       [N N N N N N]
                       [A N N N A N]
                       [A A N A N N]
                       [N A A N N N]
                       [A A A A N N]])
    (def test-board-2 [[A N N N N N]
                       [A N N N N A]
                       [A N N N B N]
                       [B B N A N N]
                       [A B A N N N]
                       [A A B A A A]])
    ;horizontal checks
    (is (= (set-check-left test-board-1 PLAYER-1 5 3) 0))
    (is (= (set-check-left test-board-1 PLAYER-1 4 3) 1))
    (is (= (set-check-left test-board-2 PLAYER-1 5 5) 3))
    ;regular diagonal checks
    (is (= (set-check-diag test-board-1 PLAYER-1 4 2) (list 2 0)))
    (is (= (set-check-diag test-board-2 PLAYER-1 4 2) (list 4 2)))
    (is (= (set-check-diag test-board-1 PLAYER-1 4 1) (list 3 0)))
    ;inverse diagonal checks
    (is (= (set-check-inv-diag test-board-1 PLAYER-1 4 2) (list 2 4)))
    (is (= (set-check-inv-diag test-board-2 PLAYER-1 4 2) (list 3 3))))

(deftest line-check
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
    (is (vert-line-check test-board-1 PLAYER-1 2 0))
    (is (not (vert-line-check test-board-2 PLAYER-1 2 0)))
    (is (not (vert-line-check test-board-3 PLAYER-2 5 0)))
    (is (not (vert-line-check test-board-3 PLAYER-2 2 0)))
    ;horizontal lines
    (is (horiz-line-check test-board-1 PLAYER-1 5 3))
    (is (not (horiz-line-check test-board-2 PLAYER-1 5 1)))
    (is (not (horiz-line-check test-board-3 PLAYER-2 5 2)))
    (is (not (horiz-line-check test-board-3 PLAYER-2 2 2)))
    ;regular diagonal lines
    (is (diag-line-check test-board-1 PLAYER-1 4 2))
    (is (not (diag-line-check test-board-2 PLAYER-1 4 2)))
    (is (not (diag-line-check test-board-3 PLAYER-2 5 2)))
    (is (not (diag-line-check test-board-3 PLAYER-2 2 2)))
    ;inverse diagonal lines
    (is (inv-diag-line-check test-board-1 PLAYER-1 4 2))
    (is (not (inv-diag-line-check test-board-2 PLAYER-1 4 2)))
    (is (not (inv-diag-line-check test-board-3 PLAYER-2 5 2)))
    (is (not (inv-diag-line-check test-board-3 PLAYER-2 2 2))))

(deftest win-condition
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
    (is (game-ended? test-board-1 PLAYER-1 4 2))
    (is (game-ended? test-board-1 PLAYER-1 5 1))
    (is (game-ended? test-board-1 PLAYER-1 5 0))
    (is (not (game-ended? test-board-2 PLAYER-1 4 2)))
    (is (not (game-ended? test-board-2 PLAYER-1 5 1)))
    (is (not (game-ended? test-board-2 PLAYER-1 5 0))))
