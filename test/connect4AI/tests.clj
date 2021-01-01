(ns connect4AI.tests (:require [connect4AI.game :refer :all]
                               [connect4AI.AI :refer :all]
                               [connect4AI.UI :refer :all]))

(require '[clojure.test :refer :all])

(def PLAYER-1 0)
(def PLAYER-2 1)

;======= GAME =========
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
    (def test-board-0 (empty-board 6 7))
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

(deftest column-top-tests
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board [[A N N A N B N]
                     [A N B A N B A]
                     [B A B A N B A]
                     [B A A A N A B]
                     [A B A B N B A]
                     [A B A B N A A]])
    (is (= (column-top test-board 0) 0))
    (is (= (column-top test-board 1) 2))
    (is (= (column-top test-board 2) 1))
    (is (= (column-top test-board 4) 6)))

;======= AI =========
(deftest setup-line-score-tests
    (def test-board (empty-board 6 7))
    ;vertical
    (is (= (setup-line-score test-board '(0 0) step-up step-down) '(0 0)))
    (is (= (setup-line-score test-board '(5 0) step-up step-down) '(0 0)))
    ;horizontal
    (is (= (setup-line-score test-board '(5 0) step-left step-right) '(5 0)))
    (is (= (setup-line-score test-board '(5 6) step-left step-right) '(5 0)))
    ;diagonal
    (is (= (setup-line-score test-board '(0 6) #(step-up (step-right %)) #(step-down (step-left %))) '(0 6)))
    (is (= (setup-line-score test-board '(5 1) #(step-up (step-right %)) #(step-down (step-left %))) '(0 6)))
    ;inverse diagonal
    (is (= (setup-line-score test-board '(1 1) #(step-up (step-left %)) #(step-down (step-right %))) '(0 0)))
    (is (= (setup-line-score test-board '(5 5) #(step-up (step-left %)) #(step-down (step-right %))) '(0 0))))

(deftest line-score-tests
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board-1 [[N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N A N N N]])
    (def test-board-2 [[N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [A N N N N N N]
                       [A N N N N N N]
                       [A A N A A N A]])
    (is (= (line-score test-board-1 0 '(5 3) step-left step-right) 1))
    (is (= (line-score test-board-2 0 '(5 3) step-left step-right) 5))
    (is (= (line-score test-board-2 0 '(5 3) step-up step-down) 1))
    (is (= (line-score test-board-2 0 '(5 0) step-up step-down) 3)))

(deftest board-score-tests
    (def A 0)
    (def B 1)
    (def N -1)
    (def test-board-1 [[N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N A N N N]])
    (def test-board-2 [[N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [A N N N N N B]])
    (def test-board-3 [[N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [A A A A N N B]])
    (def test-board-4 [[N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [N N N N N N N]
                       [A N N B B B B]])
    (is (= (get-score test-board-1 0 0) 4))
    (is (= (get-score test-board-2 1 0) 0))
    (is (= (get-score test-board-3 0 0) Integer/MAX_VALUE))
    (is (= (get-score test-board-4 1 5) Integer/MAX_VALUE)))

;============ UI ==========
(deftest playable
    (def board (empty-board 6 7))
    (is (connect4-game #(connect4-AI-move %1 %2 %3 1) #(connect4-AI-move %1 %2 %3 1) false))
    (is (connect4-game random-move random-move false))
    (is (connect4-game random-move #(connect4-AI-move %1 %2 %3 1) false)))

(deftest AI-validity
    (def board (empty-board 6 7))
    (def nplays 100)
    (is (> (count (for [i (range nplays)
                       :when (= (get (connect4-game random-move #(connect4-AI-move %1 %2 %3 2) false) :winner) 1)] 0)))
           (* nplays 0.75)))
