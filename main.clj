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

(def pos-to-char {-1 "_" 0 "A" 1 "B"})

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
    ([player-1 player-2 print-mode]
        (let [board (game/empty-board 6 7)]
             (when print-mode
                 (display-board board)
                 (println "player 1 move"))
             (connect4-game player-1 player-2 print-mode 0 board (list))))
    ([player-1 player-2 print-mode turn board move-list]
        (let [next-turn (bit-xor turn 1)
              player (if (= turn 0) player-1 player-2)
              move (if (empty? move-list)
                       (player board turn 0)
                       (player board turn (first move-list)))
              new-board (game/insert-piece board turn move)]
             (cond
                 (game/game-ended? new-board turn (list (ai/column-top new-board move) move)) {:winner turn :moves move-list :board new-board}
                 (game/board-full? board) {:winner -1 :moves move-list :board new-board}
                 :else (do
                        (when print-mode
                              (display-board new-board)
                              (println "player" (+ next-turn 1) "move"))
                        (connect4-game player-1 player-2 print-mode next-turn new-board (conj move-list move)))))))

;(prn (connect4-game human-move #(ai/connect4-AI-move %1 %2 %3 2) true))
(prn (connect4-game #(ai/connect4-AI-move %1 %2 %3 6) #(ai/connect4-AI-move %1 %2 %3 6) true))

;(prn (/ (count (for [i (range 100) :when (= (get (connect4-game ai/random-move #(ai/connect4-AI-move %1 %2 %3 2)) :winner) 1)] i)) 100.0))
;(prn (/ (count (for [i (range 100) :when (= (get (connect4-game #(ai/connect4-AI-move %1 %2 %3 2)
;                                                                #(ai/connect4-AI-move %1 %2 %3 2) false) :winner) 1)] i)) 100.0))
