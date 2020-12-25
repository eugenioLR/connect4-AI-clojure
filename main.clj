(ns main (:require [game :as game]
                   [AI :as ai]))

(defn human-move [board & not-used]
    (try
        (let [move (- (Integer. (read-line)) 1)]
            (if (contains? (set (game/gen-legal-moves board)) move)
                move
                (do
                    (println "that's not a legal move, try again")
                    (human-move board))))
        (catch NumberFormatException n
            (do
                (println "that position is not availble")
                (human-move board)))))

(defn pos-to-char [slot]
    (cond
        (not (get slot :filled)) "N"
        (= (get slot :player) 0) "A"
        :else "B"))

(defn display-board [board]
        (when (not (empty? board))
            (do
                (print (str "[" (pos-to-char (first (first board)))))
                (doseq [i (rest (first board))] (print "" (pos-to-char i)))
                (println "]")
                (display-board (rest board)))))


;(define (display-matrix mat)
;  (when (not (empty? mat))
;    (begin
;      (map display (list "(" (car (car mat))))
;      (map (lambda (x) (map display (list " " x))) (cdr (car mat)))
;      (display ")\n")
;      (display-matrix (cdr mat)))))

(defn connect4-game
    ([player-1 player-2] (let [board (game/empty-board 6 7)]
                            (when (= player-1 human-move)
                                  (display-board board)
                                  (println "player 1 move"))
                            (connect4-game player-1 player-2 0 board (list))))
    ([player-1 player-2 turn board move-list]
        (let [next-turn (bit-xor turn 1)
              player (if (= turn 0) player-1 player-2)
              move (player board turn (first move-list))
              new-board (game/insert-piece board turn move)]
             (cond
                 (game/game-ended? new-board turn (list (ai/column-top new-board move) move)) {:winner turn :moves move-list :board new-board}
                 (game/board-full? board) {:winner -1 :moves move-list :board new-board}
                 :else (do
                        (display-board new-board)
                        (println "player" (+ next-turn 1) "move")
                        (connect4-game player-1 player-2 next-turn new-board (conj move-list move)))))))

(prn (connect4-game human-move #(ai/connect4-AI-move %1 %2 %3 3)))
