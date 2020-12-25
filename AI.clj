(ns connect4AI.AI (:require [connect4AI.game :as game]))
(require '[clojure.test :refer :all])

(defn gen-legal-moves [board]
    (for [x (range (count (get board 0)))
          :when (not (get (get (get board 0) x) :filled))]
        x))

(defn column-top
    ([board column] (column-top board column 0))
    ([board column i] (if (get (get (get board i) column) :filled)
                          i
                          (column-top board column (+ i 1)))))

(defn get-score [board player first-player last-move]
    (if (game/game-ended? board player (column-top board) last-move)
        (if (= player first-player)
            Integer/MAX_VALUE
            Integer/MIN_VALUE)
        (reduce + (for [i (count board) j (count (get board 0))])))

(defn gen-minimax
    ([board player max-depth last-move] (gen-minimax board player max-depth 0 0 (list) last-move))
    ([board player max-depth last-move first-player depth]
        (let
            [new-depth (if (= player first-player) (+ depth 1) depth)]
            [new-player (bit-xor player 1)]
            (if (< depth max-depth)
                (list (list 0 board) (for [i (gen-legal-moves board)]
                                        (gen-minmax (game/insert-piece board player i) new-player max-depth i first-player new-depth)))
                (list (get-score board player last-move) board))



;======= TESTS =========
(deftest legal-moves
    (def A {:player game/PLAYER-1 :filled true})
    (def B {:player game/PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
    (def test-board-0 (game/empty-board 6))
    (def test-board-1 [[A N N A N B]
                       [A N B A N B]
                       [B A B A N B]
                       [B A A A N A]
                       [A B A B N B]
                       [A B A B N A]])
    (is (= (gen-legal-moves test-board-0) (range 6))
    (is (= (gen-legal-moves test-board-1) '(1 2 4)))))

(run-tests)
