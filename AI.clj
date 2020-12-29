(ns AI (:require [game :as game]))

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
    [board player last-move]
        (let [oponent (bit-xor player 1)]
            (if (game/game-ended? board player (list (game/column-top board last-move) last-move))
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
    [board player last-move depth]
        (let [new-player (bit-xor player 1)]
            (if (and (> depth 0)
                     (not (game/game-ended? board player (list (game/column-top board last-move) last-move))))
                (cons {:score 0 :move last-move}
                      (for [i (game/gen-legal-moves board)]
                           (gen-minimax (game/insert-piece board new-player i) new-player i (- depth 1))))
                {:score (get-score board player last-move) :move last-move})))


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

(defn connect4-AI-move [board player last-move level] (get (get-minimax-move (gen-minimax board player last-move level) player) :move))
(defn random-move [board & not-used]
    (let [moves (game/gen-legal-moves board)]
        (if (empty? moves)
            -1
            (rand-nth moves))))
