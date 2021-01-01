(ns connect4AI.UI (:require [connect4AI.game :refer :all]
                            [connect4AI.AI :refer :all])
                  (:gen-class))

(defn human-move [board & not-used]
    (try
        (let [move (- (Integer. (read-line)) 1)]
            (if (contains? (set (gen-legal-moves board)) move)
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

(defn connect4-game
    ([player-1 player-2 print-mode]
        (let [board (empty-board 6 7)]
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
              new-board (insert-piece board turn move)
              new-move-list (conj move-list move)]
             (cond
                 (game-ended? new-board turn (list (column-top new-board move) move)) {:winner turn :moves new-move-list :board new-board}
                 (board-full? board) {:winner -1 :moves new-move-list :board new-board}
                 :else (do
                        (when print-mode
                              (display-board new-board)
                              (println "player" (+ next-turn 1) "move"))
                        (connect4-game player-1 player-2 print-mode next-turn new-board new-move-list))))))
