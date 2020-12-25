(ns AI (:require [game :as game]))
(require '[clojure.test :refer :all])
(require '[clojure.core.reducers :as r])

(defn gen-legal-moves [board]
    (for [x (range (count (get board 0)))
          :when (not (get (get (get board 0) x) :filled))]
        x))

(defn column-top
    ([board column]
        (column-top board column 0))
    ([board column i]
        (if (or (get (get (get board i) column) :filled)
                (>= i (count board)))
            i
            (column-top board column (+ i 1)))))

;move cursor to the start of the line
(defn setup-line-score [board coord step rev]
    (let [next (step coord) y (first next) x (last next)]
        (if (and (>= y 0) (< y (count board)) (>= x 0) (< x (count (get board 0))))
            (setup-line-score board next step rev)
            coord)))

;go through the line and update the values acordingly:
;empty:          posible +1
;current player: posible +1, placed +1
;oposite player: posible =0, placed =0, if posible > 4: total +placed
(defn line-score
    ([board player coord step reverse]
        (line-score board player (setup-line-score board coord reverse step) step reverse 0 0 0))
    ([board player coord step reverse possible placed total]
        (let [y (first coord)
              x (last coord)
              new-possible (cond
                             (or (= (get (get (get board y) x) :player) player)
                                 (not (get (get (get board y) x) :filled)))
                                 (+ possible 1)
                             (not= (get (get (get board y) x) :player) player)
                                  0
                             :else possible)
              new-placed (cond
                            (and (= (get (get (get board y) x) :player) player)
                                 (get (get (get board y) x) :filled))
                                 (+ placed 1)
                            (get (get (get board y) x) :filled)
                                 0
                            :else placed)
              new-total (if (and (not= (get (get (get board y) x) :player) player)
                                 (get (get (get board y) x) :filled)
                                 (>= possible game/SLOT-AMOUNT))
                            (+ total placed)
                            total)]
             (if (and (>= y 0) (< y (count board)) (>= x 0) (< x (count (get board 0))))
                 (line-score board player (step coord) step reverse new-possible new-placed new-total)
                  (if (>= possible game/SLOT-AMOUNT)
                      (+ total placed)
                      total)))))

(defn vert-line-score [board player coord] (line-score board player coord game/step-up game/step-down))
(defn horiz-line-score [board player coord] (line-score board player coord game/step-right game/step-left))
(defn diag-line-score [board player coord] (line-score board player coord #(game/step-up (game/step-right %)) #(game/step-down (game/step-left %))))
(defn inv-diag-line-score [board player coord] (line-score board player coord #(game/step-up (game/step-left %)) #(game/step-down (game/step-right %))))


;heuristic for board: sum of heuristic of each of our slots, substract the score from the oponent
;heuristic for slot: for each possible line we add the pieces already inserted
(defn get-score
    [board player last-move first-player]
        (let [oponent (bit-xor player 1)]
            (if (game/game-ended? board player (list (column-top board last-move) last-move))
                (if (= player first-player)
                    Integer/MAX_VALUE
                    Integer/MIN_VALUE)
                (-
                    (reduce + (for [i (range (count board))
                                    j (range (count (get board 0)))
                                    :when (and (= (get (get (get board i) j) :player) player)
                                               (get (get (get board i) j) :filled))]
                                   (+ (vert-line-score board player (list i j))
                                      (horiz-line-score board player (list i j))
                                      (diag-line-score board player (list i j))
                                      (inv-diag-line-score board player (list i j)))))
                    (reduce + (for [i (range (count board))
                                    j (range (count (get board 0)))
                                    :when (and (= (get (get (get board i) j) :player) oponent)
                                               (get (get (get board i) j) :filled))]
                                    (+ (vert-line-score board oponent (list i j))
                                       (horiz-line-score board oponent (list i j))
                                       (diag-line-score board oponent (list i j))
                                       (inv-diag-line-score board oponent (list i j)))))))))

(defn gen-minimax
    ([board player max-depth last-move]
        (gen-minimax board player max-depth last-move player 0))
    ([board player max-depth last-move first-player depth]
        (let
            [new-depth (if (= player first-player) (+ depth 1) depth)
             new-player (bit-xor player 1)]
            (if (< depth max-depth)
                (cons {:score 0 :move last-move}
                      (for [i (gen-legal-moves board)]
                           (gen-minimax (game/insert-piece board new-player i) new-player max-depth i first-player new-depth)))
                {:score (get-score board player first-player last-move) :move last-move}))))


(defn get-minimax-move
    ([tree player] (get-minimax-move tree player player))
    ([tree player first-player]
        (let [min-or-max (if (= player first-player) max min)
              new-player (bit-xor player 1)
              max-or-min (if (= player first-player) min-key max-key)]
             (if (get tree :score) ;if it's a leaf node
                 tree
                 (apply max-or-min #(get % :score) (for [i (rest tree)]
                                                        (get-minimax-move i new-player first-player)))))))

(defn connect4-AI [board player level last-move] (get-minimax-move (gen-minimax board player level last-move) player (bit-xor player 1)))


;======= TESTS =========
(deftest legal-moves-tests
    (def A {:player game/PLAYER-1 :filled true})
    (def B {:player game/PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
    (def test-board-0 (game/empty-board 6 7))
    (def test-board-1 [[A N N A N B N]
                       [A N B A N B A]
                       [B A B A N B A]
                       [B A A A N A B]
                       [A B A B N B A]
                       [A B A B N A A]])
    (is (= (gen-legal-moves test-board-0) (range 7)))
    (is (= (gen-legal-moves test-board-1) '(1 2 4 6))))

(deftest column-top-tests
    (def A {:player game/PLAYER-1 :filled true})
    (def B {:player game/PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
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

(deftest setup-line-score-tests
    (def test-board (game/empty-board 6 7))
    ;vertical
    (is (= (setup-line-score test-board '(0 0) game/step-up game/step-down) '(0 0)))
    (is (= (setup-line-score test-board '(5 0) game/step-up game/step-down) '(0 0)))
    ;horizontal
    (is (= (setup-line-score test-board '(5 0) game/step-left game/step-right) '(5 0)))
    (is (= (setup-line-score test-board '(5 6) game/step-left game/step-right) '(5 0)))
    ;diagonal
    (is (= (setup-line-score test-board '(0 6) #(game/step-up (game/step-right %)) #(game/step-down (game/step-left %))) '(0 6)))
    (is (= (setup-line-score test-board '(5 1) #(game/step-up (game/step-right %)) #(game/step-down (game/step-left %))) '(0 6)))
    ;inverse diagonal
    (is (= (setup-line-score test-board '(1 1) #(game/step-up (game/step-left %)) #(game/step-down (game/step-right %))) '(0 0)))
    (is (= (setup-line-score test-board '(5 5) #(game/step-up (game/step-left %)) #(game/step-down (game/step-right %))) '(0 0))))

(deftest line-score-tests
    (def A {:player game/PLAYER-1 :filled true})
    (def B {:player game/PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
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
    (is (= (line-score test-board-1 0 '(5 3) game/step-left game/step-right) 1))
    (is (= (line-score test-board-2 0 '(5 3) game/step-left game/step-right) 5))
    (is (= (line-score test-board-2 0 '(5 3) game/step-up game/step-down) 1))
    (is (= (line-score test-board-2 0 '(5 0) game/step-up game/step-down) 3)))

(deftest board-score-tests
    (def A {:player game/PLAYER-1 :filled true})
    (def B {:player game/PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
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
    (is (= (get-score test-board-1 0 0 0) 4))
    (is (= (get-score test-board-2 1 0 0) 0))
    (is (= (get-score test-board-3 0 0 0) Integer/MAX_VALUE))
    (is (= (get-score test-board-4 1 5 0) Integer/MIN_VALUE)))

(run-tests)
