(ns AI (:require [game :as game]))
(require '[clojure.test :refer :all])

(defn column-top
    ([board column]
        (column-top board column 0))
    ([board column i]
        (if (or (not= (get (get board i) column) -1)
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
              oponent (bit-xor player 1)
              new-possible (if (not= (get (get board y) x) oponent)
                               (+ possible 1)
                               0)
              new-placed (cond
                            (= (get (get board y) x) player)
                               (+ placed 1)
                            (= (get (get board y) x) oponent)
                               0
                            :else placed)
              new-total (if (and (>= possible game/SLOT-AMOUNT)
                                 (= (get (get board y) x) oponent))
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
                Integer/MAX_VALUE
                (-
                    (reduce + (for [i (range (count board))
                                    j (range (count (get board 0)))
                                    :when (= (get (get board i) j) player)]
                                   (+ (vert-line-score board player (list i j))
                                      (horiz-line-score board player (list i j))
                                      (diag-line-score board player (list i j))
                                      (inv-diag-line-score board player (list i j)))))
                    (reduce + (for [i (range (count board))
                                    j (range (count (get board 0)))
                                    :when (= (get (get board i) j) oponent)]
                                    (+ (vert-line-score board oponent (list i j))
                                       (horiz-line-score board oponent (list i j))
                                       (diag-line-score board oponent (list i j))
                                       (inv-diag-line-score board oponent (list i j)))))))))

(defn gen-minimax
    ([board player max-depth last-move]
        (gen-minimax board player max-depth last-move player 0))
    ([board player max-depth last-move first-player depth]
        (let [new-player (bit-xor player 1)]
            (if (and (< depth max-depth)
                     (not (game/game-ended? board player (list (column-top board last-move) last-move))))
                (cons {:score 0 :move last-move}
                      (for [i (game/gen-legal-moves board)]
                           (gen-minimax (game/insert-piece board new-player i) new-player max-depth i first-player (+ depth 1))))
                {:score (get-score board player first-player last-move) :move last-move}))))


(defn get-minimax-move
    ([tree player] (get-minimax-move tree (bit-xor player 1) player))
    ([tree player first-player]
        (let [new-player (bit-xor player 1)
              max-or-min (if (= player first-player) min-key max-key)]
             (if (get tree :score) ;if it's a leaf node
                 tree
                 (if (empty? (rest tree))
                     (first tree)
                     (apply max-or-min #(get % :score) (for [i (rest tree)]
                                                            (get-minimax-move i new-player first-player))))))))

(defn connect4-AI-move [board player last-move level] (get (get-minimax-move (gen-minimax board player level last-move) player) :move))
(defn random-move [board & not-used] (rand-nth (game/gen-legal-moves board)))

;======= TESTS =========
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
    (is (= (line-score test-board-1 0 '(5 3) game/step-left game/step-right) 1))
    (is (= (line-score test-board-2 0 '(5 3) game/step-left game/step-right) 5))
    (is (= (line-score test-board-2 0 '(5 3) game/step-up game/step-down) 1))
    (is (= (line-score test-board-2 0 '(5 0) game/step-up game/step-down) 3)))

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
    (is (= (get-score test-board-1 0 0 0) 4))
    (is (= (get-score test-board-2 1 0 0) 0))
    (is (= (get-score test-board-3 0 0 0) Integer/MAX_VALUE))
    (is (= (get-score test-board-4 1 5 0) Integer/MIN_VALUE)))

;(run-tests)
