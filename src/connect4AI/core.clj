(ns connect4AI.core (:require [connect4AI.AI :refer :all]
                             [connect4AI.UI :refer :all])
                    (:gen-class))

(defn display-results [results]
    (do
        (print "\n\n\n\n")
        (case (results :winner)
              -1 (println "DRAW")
              0  (println "PLAYER 1 WINS")
              1  (println "PLAYER 2 WINS")
              (println "Something weird happened..."))
        (println "final board state:")
        (display-board (results :board))))

(defn -main [& args]
    (if (> (count args) 0)
        (try
            (let [level (Integer. (first args))]
                 (if (> level 0)
                     (display-results (connect4-game human-move #(connect4-AI-move %1 %2 %3 level) true))
                     (display-results (connect4-game human-move random-move true))))
        (catch NumberFormatException n
            (println "The AI level must be a number.")))
        (display-results (connect4-game human-move #(connect4-AI-move %1 %2 %3 1) true))))


;(prn (connect4-game #(connect4-AI-move %1 %2 %3 3) #(connect4-AI-move %1 %2 %3 3) true))
;(prn (/ (count (for [i (range 100) :when (= (get (connect4-game random-move #(connect4-AI-move %1 %2 %3 4) false) :winner) 1)] i)) 100.0))
;(prn (/ (count (for [i (range 100) :when (= (get (connect4-game #(connect4-AI-move %1 %2 %3 2)
;                                                                #(connect4-AI-move %1 %2 %3 2) false) :winner) 1)] i)) 100.0))
